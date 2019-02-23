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
import java.nio.file.StandardWatchEventKinds.OVERFLOW
import java.nio.file.{ Files, Path, WatchKey }
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }
import java.util.concurrent.{ ConcurrentHashMap, CountDownLatch, TimeUnit }

import FileTreeView.AllPass
import sbt.io._
import sbt.io.syntax._

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Success

private[sbt] object WatchServiceBackedObservable {
  private val eventThreadId = new AtomicInteger(0)
}
import sbt.internal.io.WatchServiceBackedObservable._
private[sbt] class WatchServiceBackedObservable[+T](s: NewWatchState,
                                                    delay: FiniteDuration,
                                                    converter: (Path, SimpleFileAttributes) => T,
                                                    closeService: Boolean,
                                                    logger: WatchLogger)
    extends Registerable[(Path, T)]
    with Observable[(Path, T)] {
  private[this] val closed = new AtomicBoolean(false)
  private[this] val observers = new Observers[(Path, T)]
  private[this] val view = FileTreeView.DEFAULT
  private[this] val thread: Thread = {
    //val entryConverter = Entry.converter(converter)
    val lock = new Object
    val latch = new CountDownLatch(1)
    new Thread(s"watch-state-event-thread-${eventThreadId.incrementAndGet()}") {
      setDaemon(true)
      start()
      if (!latch.await(5, TimeUnit.SECONDS))
        throw new IllegalStateException("Couldn't start event thread")
      @tailrec
      final def loopImpl(): Unit = {
        if (!closed.get) getFilesForKey(s.service.poll(delay)).foreach {
          case (path, attrs) =>
            Evaluate(converter(path, attrs)).foreach(t => observers.onNext(path -> t))
        }
        if (!closed.get) loopImpl()
      }
      override def run(): Unit = {
        latch.countDown()
        try {
          loopImpl()
        } catch {
          case _: InterruptedException =>
        }
      }

      def getFilesForKey(key: WatchKey): Vector[(Path, SimpleFileAttributes)] = key match {
        case null => Vector.empty
        case k =>
          val rawEvents = k.synchronized {
            val events = k.pollEvents.asScala.toVector
            k.reset()
            events
          }
          val keyPath = k.watchable.asInstanceOf[Path]
          val allEvents: Seq[(Path, SimpleFileAttributes)] = rawEvents.flatMap {
            case e if e.kind.equals(OVERFLOW) =>
              handleOverflow(k).toSeq
            case e if !e.kind.equals(OVERFLOW) && e.context != null =>
              val path = keyPath.resolve(e.context.asInstanceOf[Path])
              SimpleFileAttributes.get(path) match {
                case Success(attrs) => Some(path -> attrs)
                case _              => None
              }
            case _ => Nil: Seq[(Path, SimpleFileAttributes)]
          }
          logger.debug(s"Received events:\n${allEvents.mkString("\n")}")
          val (exist, notExist) = allEvents.partition(_._2.exists)
          val (updatedDirectories, updatedFiles) = exist.partition(_._2.isDirectory)
          val newFiles: Seq[(Path, SimpleFileAttributes)] =
            updatedDirectories.flatMap(filesForNewDirectory.tupled)
          lock.synchronized { s.registered --= notExist.map(_._1) }
          notExist.foreach { case (p, _) => s.unregister(p) }
          updatedDirectories.toVector ++ updatedFiles ++ newFiles ++ notExist
      }

      /*
       * In the case of an overflow, we must poll the file system to find out if there are added
       * or removed directories. When there are new directories, we also want to return file
       * events for the files that are found therein. Because an overflow is likely to occur while
       * a directory is still being modified, we poll repeatedly until we get the same list of
       * files consecutively. We will not trigger for any files that are updated while the WatchKey
       * is in the OVERFLOW state. There is no good way to fix this without caching mtimes for
       * all of the files, which I don't think is worth doing at this juncture.
       */
      private def handleOverflow(key: WatchKey): Iterable[(Path, SimpleFileAttributes)] =
        lock.synchronized {
          val allFiles = new ConcurrentHashMap[Path, SimpleFileAttributes].asScala
          def addNewFiles(): Unit = {
            allFiles.clear()
            val view = FileTreeView.DEFAULT
            view.list(key.watchable.asInstanceOf[Path] ** AllPassFilter, AllPass).foreach {
              case (path, attrs) =>
                allFiles += path -> attrs
                if (attrs.isDirectory && !s.registered.contains(path)) {
                  s.register(path)
                }
            }
            ()
          }

          var oldFiles = new ConcurrentHashMap[Path, SimpleFileAttributes].asScala
          do {
            oldFiles = allFiles
            addNewFiles()
          } while (oldFiles != allFiles)
          s.registered --= s.registered.collect {
            case (d, k) if !Files.exists(d) =>
              k.reset()
              k.cancel()
              d
          }
          allFiles
        }

      /*
       * Returns new files found in new directory and any subdirectories, assuming that there is
       * a recursive source with a base that is parent to the directory.
       */
      private val filesForNewDirectory
        : (Path, SimpleFileAttributes) => Seq[(Path, SimpleFileAttributes)] =
        (dir: Path, _) =>
          if (!closed.get()) {
            lazy val recursive =
              s.globs.exists(glob => dir.startsWith(glob.base) && glob.depth > 0)
            if (!s.registered.contains(dir) && recursive) {
              s.register(dir)
              @tailrec
              def poll(paths: Seq[Path] = Nil): Seq[(Path, SimpleFileAttributes)] = {
                val typedPaths = FileTreeView.DEFAULT.list(dir ** AllPassFilter, AllPass)
                val newPaths = typedPaths.map(_._1)
                if (newPaths == paths) typedPaths else poll(newPaths)
              }
              val result = poll()
              result.foreach {
                case (path, attrs) if attrs.isDirectory && !closed.get() => path -> s.register(path)
                case _                                                   =>
              }
              result.toVector
            } else Nil
          } else Nil
    }
  }
  override def addObserver(observer: Observer[(Path, T)]): AutoCloseable =
    observers.addObserver(observer)

  override def close(): Unit = {
    if (closed.compareAndSet(false, true)) {
      thread.interrupt()
      thread.join(5.seconds.toMillis)
      if (closeService) s.service.close()
      logger.debug("Closed WatchServiceBackedObservable")
    }
  }

  override def register(glob: Glob): Either[IOException, Observable[(Path, T)]] = {
    try {
      s.register(glob.base)
      view
        .list(glob.withFilter(AllPassFilter), _._2.isDirectory)
        .foreach(r => s.register(r._1))
      new RegisterableObservable(observers).register(glob)
    } catch {
      case e: IOException => Left(e)
    }
  }
}
