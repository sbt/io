/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.nio

import java.io.IOException
import java.nio.file.StandardWatchEventKinds.OVERFLOW
import java.nio.file.{ Path, WatchKey }
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }
import java.util.concurrent.{ CountDownLatch, TimeUnit }

import sbt.internal.io._
import sbt.internal.nio.FileEvent.{ Creation, Deletion }
import sbt.nio.file.FileAttributes.NonExistent
import sbt.nio.file.{ FileAttributes, FileTreeView, Glob }
import sbt.nio.file.Glob._
import sbt.nio.file.Glob.GlobOps._
import sbt.nio.filters.AllPass

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.control.NonFatal

private[sbt] object WatchServiceBackedObservable {
  private val eventThreadId = new AtomicInteger(0)
}
private[sbt] class WatchServiceBackedObservable(s: NewWatchState,
                                                delay: FiniteDuration,
                                                closeService: Boolean,
                                                logger: WatchLogger)
    extends Registerable[FileEvent[FileAttributes]]
    with Observable[FileEvent[FileAttributes]] {
  import WatchServiceBackedObservable.eventThreadId
  private[this] type Event = FileEvent[FileAttributes]
  private[this] val closed = new AtomicBoolean(false)
  private[this] val observers = new Observers[FileEvent[FileAttributes]]
  private[this] val fileCache = new FileCache(p => FileAttributes(p).getOrElse(NonExistent))
  private[this] val view: FileTreeView.Nio[FileAttributes] = FileTreeView.default
  private[this] val thread: Thread = {
    val latch = new CountDownLatch(1)
    new Thread(s"watch-state-event-thread-${eventThreadId.incrementAndGet()}") {
      setDaemon(true)
      start()
      if (!latch.await(5, TimeUnit.SECONDS))
        throw new IllegalStateException("Couldn't start event thread")
      @tailrec
      final def loopImpl(): Unit = {
        try {
          if (!closed.get) getFilesForKey(s.service.poll(delay)).foreach { event =>
            observers.onNext(event)
          }
        } catch {
          case NonFatal(e) =>
            logger.debug(
              s"Error getting files from ${s.service}: $e\n${e.getStackTrace mkString "\n"}")
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

      def getFilesForKey(key: WatchKey): Seq[Event] = key match {
        case null => Vector.empty
        case k =>
          val rawEvents = k.synchronized {
            val events = k.pollEvents.asScala.toVector
            k.reset()
            events
          }
          val keyPath = k.watchable.asInstanceOf[Path]
          val allEvents: Seq[Event] = rawEvents.flatMap {
            case e if e.kind.equals(OVERFLOW) =>
              fileCache.refresh(keyPath / "**")
            case e if !e.kind.equals(OVERFLOW) && e.context != null =>
              val path = keyPath.resolve(e.context.asInstanceOf[Path])
              FileAttributes(path) match {
                case Right(attrs) => fileCache.update(path, attrs)
                case _            => Nil
              }
            case _ => Nil: Seq[Event]
          }
          logger.debug(s"Received events:\n${allEvents.mkString("\n")}")
          def process(event: Event): Seq[Event] = {
            (event match {
              case Creation(path, attrs) if attrs.isDirectory =>
                s.register(path)
                event +: view.list(Glob(path, (1, Int.MaxValue), AllPass)).flatMap {
                  case (p, a) =>
                    process(Creation(p, a))
                }
              case Deletion(p, attrs) if attrs.isDirectory =>
                val events = fileCache.refresh(p / "**")
                events.view.filter(_.attributes.isDirectory).foreach(e => s.unregister(e.path))
                events
              case e => e :: Nil
            }).map {
              case d @ Deletion(path, attributes) =>
                val newAttrs = FileAttributes(isDirectory = attributes.isDirectory,
                                              isOther = false,
                                              isRegularFile = attributes.isRegularFile,
                                              isSymbolicLink = attributes.isSymbolicLink)
                Deletion(path, newAttrs, d.occurredAt)
              case e => e
            }
          }
          allEvents.flatMap(process)
      }

    }
  }
  override def addObserver(observer: Observer[FileEvent[FileAttributes]]): AutoCloseable =
    observers.addObserver(observer)

  override def close(): Unit = {
    if (closed.compareAndSet(false, true)) {
      thread.interrupt()
      thread.join(5.seconds.toMillis)
      if (closeService) s.service.close()
      logger.debug("Closed WatchServiceBackedObservable")
    }
  }

  override def register(glob: Glob): Either[IOException, Observable[FileEvent[FileAttributes]]] = {
    try {
      fileCache.register(glob)
      fileCache.list(Glob(glob.base, glob.range, AllPass)) foreach {
        case (path, attrs) if attrs.isDirectory => s.register(path)
        case _                                  =>
      }
      val observable = new Observers[FileEvent[FileAttributes]] {
        override def onNext(t: FileEvent[FileAttributes]): Unit = {
          if (glob.toFileFilter.accept(t.path.toFile)) super.onNext(t)
        }
      }
      val handle = observers.addObserver(observable)
      Right(new Observable[FileEvent[FileAttributes]] {
        override def addObserver(observer: Observer[FileEvent[FileAttributes]]): AutoCloseable =
          observable.addObserver(observer)
        override def close(): Unit = handle.close()
        override def toString = s"Observable($glob)"
      })
    } catch {
      case e: IOException => Left(e)
    }
  }
}
