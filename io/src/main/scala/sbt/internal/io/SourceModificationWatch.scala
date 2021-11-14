/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.io

import java.io.IOException
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.{ WatchService => _, _ }
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import sbt.internal.nio._
import sbt.io._
import sbt.io.syntax._
import sbt.nio.file.{ AnyPath, FileAttributes, FileTreeView, Glob, RecursiveGlob }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.{ immutable, mutable }
import scala.concurrent.duration._

private[sbt] object SourceModificationWatch {

  /**
   * Checks for modifications on the file system every `delay`,
   * until changes are detected or `terminationCondition` evaluates to `true`.
   * Uses default anti-entropy time of 40.milliseconds.
   */
  @deprecated("This is superseded by FileEventMonitor.poll", "1.1.7")
  def watch(delay: FiniteDuration, state: WatchState)(
      terminationCondition: => Boolean
  ): (Boolean, WatchState) = {
    if (state.count == 0) {
      (true, state.withCount(1))
    } else {
      val observable: Observable[FileEvent[FileAttributes]] =
        new WatchServiceBackedObservable(
          state.toNewWatchState,
          delay,
          closeService = false,
          NullWatchLogger
        )
      val monitor: FileEventMonitor[FileEvent[FileAttributes]] =
        FileEventMonitor.antiEntropy(
          observable,
          200.milliseconds,
          NullWatchLogger,
          50.milliseconds,
          10.minutes
        )
      @tailrec
      def poll(): Boolean = {
        monitor.poll(10.millis) match {
          case sources if sources.nonEmpty => true
          case _ if terminationCondition   => false
          case _                           => poll()
        }
      }
      try {
        val triggered = poll()
        (triggered, state.withCount(state.count + 1))
      } finally {
        monitor.close()
      }
    }
  }
}

/** The state of the file watch. */
@deprecated("WatchState is no longer used in continuous builds", "1.3.0")
private[sbt] final class WatchState private (
    val count: Int,
    private[sbt] val sources: Seq[Source],
    private[sbt] val service: WatchService,
    private[sbt] val registered: Map[Path, WatchKey]
) extends AutoCloseable {
  private[this] val closed = new AtomicBoolean(false)
  def accept(p: Path): Boolean = sources.exists(_.accept(p))
  def unregister(path: Path): Unit = service match {
    case s: Unregisterable => s.unregister(path)
    case _                 =>
  }

  /** Removes all of `fs` from the watched paths. */
  private[sbt] def --(fs: Iterable[Path]): WatchState = {
    for {
      f <- fs
      wk <- registered.get(f)
      if registered.values.count(_ == wk) <= 1
    } unregister(wk.watchable().asInstanceOf[Path])
    withRegistered(registered -- fs)
  }

  /** Adds all of `fs` to the watched paths. */
  private[sbt] def ++(fs: Iterable[Path]): WatchState = {
    val newKeys =
      fs.filter(Files.exists(_)).foldLeft(registered) {
        case (ks, d) if Files.isDirectory(d) =>
          if (ks.contains(d)) ks
          else ks + (d -> register(d))
        case (ks, f) =>
          val parent = f.getParent
          if (!ks.contains(parent)) ks + (parent -> register(parent))
          else ks
      }
    withRegistered(newKeys)
  }

  /** Retrieve events from the `WatchService` */
  private[sbt] def pollEvents(): Iterable[(Path, WatchEvent[_])] = {
    val events = service.pollEvents()
    events.toIterable.flatMap {
      case (k, evs) => evs.map((k.watchable().asInstanceOf[Path], _))
    }
  }

  /** register a path with the watch service */
  private[sbt] def register(path: Path): WatchKey = service.register(path, WatchState.events: _*)

  /** A new state, with a new `count`. */
  private[sbt] def withCount(count: Int): WatchState =
    new WatchState(count, sources, service, registered)

  /** A new state, with new keys registered. */
  private[sbt] def withRegistered(registered: Map[Path, WatchKey]): WatchState =
    new WatchState(count, sources, service, registered)

  /** Shuts down the watch service. */
  override def close(): Unit = if (closed.compareAndSet(false, true)) {
    service.close()
  }
  private[sbt] def toNewWatchState: NewWatchState = {
    val globs = ConcurrentHashMap.newKeySet[Glob].asScala
    globs ++= sources.map { s =>
      val base = if (s.recursive) Glob(s.base, RecursiveGlob) else Glob(s.base, AnyPath)
      // TODO fix this
      base
    }
    val map = new ConcurrentHashMap[Path, WatchKey]()
    map.putAll(registered.asJava)
    new NewWatchState(globs, service, map.asScala)
  }
}

private[sbt] class NewWatchState(
    private[sbt] val globs: mutable.Set[Glob],
    private[sbt] val service: WatchService,
    private[sbt] val registered: mutable.Map[Path, WatchKey]
) extends AutoCloseable {
  private[sbt] def register(path: Path): WatchKey =
    try {
      registered.get(path) match {
        case Some(k) => k
        case None =>
          val key = service.register(path, WatchState.events: _*)
          registered.put(path, key).foreach { k =>
            k.reset()
            k.cancel()
          }
          key
      }
    } catch {
      case _: IOException => null
    }
  private[sbt] def unregister(path: Path): Unit = registered.remove(path).foreach(_.cancel())
  override def close(): Unit = {
    registered.values.foreach { k =>
      k.reset()
      k.cancel()
    }
    registered.clear()
    service.close()
  }
}

/**
 * Represents how to acquire a list of items to watch.
 * @param base          Where to start looking for files.
 * @param includeFilter Filter to apply to determine whether to include a file.
 * @param excludeFilter Filter to apply to determine whether to ignore a file.
 * @param recursive     Whether the lists is recursive or immediate children.
 */
final class Source(
    val base: File,
    val includeFilter: FileFilter,
    val excludeFilter: FileFilter,
    val recursive: Boolean
) {

  def this(base: File, includeFilter: FileFilter, excludeFilter: FileFilter) =
    this(base, includeFilter, excludeFilter, true)

  /**
   * Determine whether `p` should be included in this source.
   * @param p           The path to test.
   * @param includeDirs Whether all directories should be included.
   * @return            True, if `p` should be included, false otherwise.
   */
  private[sbt] def accept(p: Path, includeDirs: Boolean = false): Boolean = {
    val inc =
      if (includeDirs) DirectoryFilter || includeFilter
      else includeFilter

    (if (!recursive) p.getParent == base.toPath else p.startsWith(base.toPath)) && inc.accept(
      p.toFile
    ) && !excludeFilter.accept(p.toFile)
  }

  /**
   * Gathers all the paths from this source without applying filters.
   * @return A sequence of all the paths collected from this source.
   */
  private[sbt] def getUnfilteredPaths(): Seq[Path] =
    FileTreeView.default.list(Glob(base, if (recursive) RecursiveGlob else AnyPath)).map(_._1)

  def withRecursive(recursive: Boolean): Source =
    new Source(base, includeFilter, excludeFilter, recursive)

  override def toString: String =
    s"""Source(
       |  base = $base,
       |  includeFilter = $includeFilter,
       |  excludeFilter = $excludeFilter,
       |  recursive = $recursive,
       |)""".stripMargin

  override def equals(o: Any): Boolean = o match {
    case that: Source =>
      this.base == that.base && this.includeFilter == that.includeFilter &&
        this.excludeFilter == that.excludeFilter && this.recursive == that.recursive
    case _ => false
  }
  override lazy val hashCode: Int = Seq[Any](base, includeFilter, excludeFilter, recursive).hashCode
}

object Source {
  def apply(base: File): Source =
    new Source(base, AllPassFilter, NothingFilter)

  def apply(base: File, includeFilter: FileFilter, excludeFilter: FileFilter): Source =
    new Source(base, includeFilter, excludeFilter)
}

private[sbt] object WatchState {

  /** What events should be monitored */
  val events: Array[WatchEvent.Kind[Path]] = Array(ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)

  /**
   * An empty `WatchState`.
   * @param service The `WatchService` to use to monitor the file system.
   * @param sources The sources from where to collect the paths.
   * @return An initial `WatchState`.
   */
  @deprecated("All apis involving Source are now superceded by glob apis", "1.3.0")
  def empty(service: WatchService, sources: Seq[Source]): WatchState = {
    val initFiles = sources.flatMap {
      case s if s.recursive =>
        val base = s.base.toPath
        if (Files.exists(base)) {
          Files.walk(base).iterator.asScala.collect {
            case d if Files.isDirectory(d) => d.toRealPath()
          }
        } else Seq(base)
      case s => Seq(s.base.toPath)
    }.sorted
    assert(initFiles.nonEmpty)
    val initState = new WatchState(count = 1, sources, service, Map.empty) ++ initFiles
    service.init()
    initState
  }

  private def toSource(glob: Glob): Source =
    new Source(glob.base.toFile, glob.toFileFilter, NothingFilter, glob.range._2 == Int.MaxValue)

  def empty(globs: Seq[Glob], service: WatchService): NewWatchState = {
    val globSet = ConcurrentHashMap.newKeySet[Glob].asScala
    globSet ++= globs
    val initFiles = globs.flatMap {
      case glob if glob.range._2 == Int.MaxValue =>
        FileTreeView.default.list(Glob(glob.base, RecursiveGlob)).collect {
          case (p, a) if a.isDirectory => p
        }
      case glob => Seq(glob.base)
    }.sorted
    service.init()
    val init = new NewWatchState(globSet, service, new ConcurrentHashMap[Path, WatchKey].asScala)
    initFiles.foreach(init.register)
    init
  }

  def empty(globs: Seq[Glob]): WatchState = {
    val service = new WatchService {
      override def init(): Unit = {}
      override def pollEvents(): Map[WatchKey, immutable.Seq[WatchEvent[Path]]] = Map.empty
      override def poll(timeout: Duration): WatchKey = null
      override def register(path: Path, events: WatchEvent.Kind[Path]*): WatchKey = null
      override def close(): Unit = {}
    }
    new WatchState(count = 1, globs.map(toSource), service, Map.empty)
  }
}
