/* sbt
 * Copyright 2009-2015 Typesafe, Inc, Mark Harrah, and others
 */
package sbt.internal.io

import java.nio.file.{ Files, Path, WatchEvent, WatchKey }
import java.nio.file.StandardWatchEventKinds._

import sbt.io.{ DirectoryFilter, FileFilter, WatchService, AllPassFilter, NothingFilter }
import sbt.io.syntax._

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

private[sbt] object SourceModificationWatch {

  /**
   * Checks for modifications on the file system every `delay`,
   * until changes are detected or `terminationCondition` evaluates to `true`.
   */
  @tailrec
  def watch(delay: FiniteDuration, state: WatchState)(
      terminationCondition: => Boolean
  ): (Boolean, WatchState) = {
    if (state.count == 0) (true, state.withCount(1))
    else {
      val events =
        state.pollEvents().map(expandEvent)

      if (events.isEmpty) {
        if (terminationCondition) {
          (false, state)
        } else {
          Thread.sleep(delay.toMillis)
          watch(delay, state)(terminationCondition)
        }
      } else {
        val previousFiles = state.registered.keySet
        val newFiles = state.sources.flatMap(_.getUnfilteredPaths()).toSet
        val createdFiles = newFiles -- previousFiles
        val deletedFiles = previousFiles -- newFiles

        // We may have events that are not relevant (e.g., created an empty directory.)
        // We filter out those changes, so that we don't trigger unnecessarily.
        val filteredDeleted = deletedFiles.filter(p => state.sources.exists(_.accept(p, false)))
        val filteredCreated = createdFiles.filter(p => state.sources.exists(_.accept(p, false)))
        val filteredModified = events.collect {
          case (p, ENTRY_MODIFY) if state.sources.exists(_.accept(p, false)) => p
        }

        // Register and remove _unfiltered_ files. This is correct because directories
        // are likely to be filtered out (for instance), but we should still add them
        // to the files that are watched.
        // We don't increment count because we don't know yet if we'll trigger.
        val newState = state ++ createdFiles -- deletedFiles

        if (filteredCreated.nonEmpty || filteredDeleted.nonEmpty || filteredModified.nonEmpty) {
          (true, newState.withCount(newState.count + 1))
        } else {
          Thread.sleep(delay.toMillis)
          watch(delay, newState)(terminationCondition)
        }
      }
    }
  }

  private def expandEvent(event: (Path, WatchEvent[_])): (Path, WatchEvent.Kind[Path]) = {
    event match {
      case (base, ev) =>
        val fullPath = base.resolve(ev.context().asInstanceOf[Path])
        val kind = ev.kind().asInstanceOf[WatchEvent.Kind[Path]]
        (fullPath, kind)
    }
  }
}

/** The state of the file watch. */
private[sbt] final class WatchState private (
    val count: Int,
    private[sbt] val sources: Seq[Source],
    service: WatchService,
    private[sbt] val registered: Map[Path, WatchKey]
) {

  /** Removes all of `fs` from the watched paths. */
  private[sbt] def --(fs: Iterable[Path]): WatchState = {
    for {
      f <- fs;
      wk <- registered.get(f);
      if (registered.values.count(_ == wk)) <= 1
    } wk.cancel()
    withRegistered(registered -- fs)
  }

  /** Adds all of `fs` to the watched paths. */
  private[sbt] def ++(fs: Iterable[Path]): WatchState = {
    val newKeys =
      fs.filter(Files.exists(_)).foldLeft(registered) {
        case (ks, d) if Files.isDirectory(d) =>
          if (ks.contains(d)) ks
          else ks + (d -> service.register(d, WatchState.events: _*))

        case (ks, f) =>
          val parent = f.getParent
          if (ks.contains(parent)) ks + (f -> ks(parent))
          else ks + (f -> service.register(parent, WatchState.events: _*))
      }
    withRegistered(newKeys)
  }

  /** Retrieve events from the `WatchService` */
  private[sbt] def pollEvents(): Iterable[(Path, WatchEvent[_])] = {
    val events = service.pollEvents
    events.toIterable.flatMap {
      case (k, evs) => evs.map((k.watchable().asInstanceOf[Path], _))
    }
  }

  /** A new state, with a new `count`. */
  private[sbt] def withCount(count: Int): WatchState =
    new WatchState(count, sources, service, registered)

  /** A new state, with new keys registered. */
  private[sbt] def withRegistered(registered: Map[Path, WatchKey]): WatchState =
    new WatchState(count, sources, service, registered)
}

/**
 * Represents how to acquire a list of items to watch.
 * @param base          Where to start looking for files.
 * @param includeFilter Filter to apply to determine whether to include a file.
 * @param excludeFilter Filter to apply to determine whether to ignore a file.
 * @param recursive     Whether the lists is recursive or immediate children.
 */
final class Source(
    base: File,
    includeFilter: FileFilter,
    excludeFilter: FileFilter,
    recursive: Boolean
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

    p.startsWith(base.toPath) && inc.accept(p.toFile) && !excludeFilter.accept(p.toFile)
  }

  /**
   * Gathers all the paths from this source without applying filters.
   * @return A sequence of all the paths collected from this source.
   */
  private[sbt] def getUnfilteredPaths(): Seq[Path] = {
    val pathFinder = if (recursive) base.allPaths else base.glob(AllPassFilter)
    pathFinder.get.map(_.toPath)
  }

  def withRecursive(recursive: Boolean): Source =
    new Source(base, includeFilter, excludeFilter, recursive)

  override def toString =
    s"""Source(
       |  base = $base,
       |  includeFilter = $includeFilter,
       |  excludeFilter = $excludeFilter,
       |  recursive = $recursive,
       |)""".stripMargin

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
  def empty(service: WatchService, sources: Seq[Source]): WatchState = {
    val initFiles = sources.flatMap(_.getUnfilteredPaths())
    assert(initFiles.nonEmpty)
    val initState = new WatchState(0, sources, service, Map.empty) ++ initFiles
    service.init()
    initState
  }

}
