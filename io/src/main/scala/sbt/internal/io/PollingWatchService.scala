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

import java.nio.file.StandardWatchEventKinds._
import java.nio.file.{
  ClosedWatchServiceException,
  WatchEvent,
  WatchKey,
  Watchable,
  Path => NioPath
}
import java.util
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{ Collections, Comparator, List => JList }

import FileTreeView.AllPass
import sbt.io._
import sbt.io.syntax._

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

/** A `WatchService` that polls the filesystem every `delay`. */
private[sbt] class PollingWatchService(delay: FiniteDuration)
    extends WatchService
    with Unregisterable {
  private[this] val closed = new AtomicBoolean(false)
  private[this] val thread = new PollingThread
  private[this] val registered = new ConcurrentHashMap[NioPath, PollingWatchKey].asScala
  private[this] var lastTimestamps: Map[NioPath, Long] = Map.empty
  private[this] val readyKeys = new LinkedBlockingQueue[PollingWatchKey]
  private[this] val view: NioFileTreeView[Long] = {
    val converter: (NioPath, SimpleFileAttributes) => Long = (path, _) =>
      IO.getModifiedTimeOrZero(path.toFile)
    FileTreeView.DEFAULT.map(converter)
  }
  private[this] val shutdownLatch = new CountDownLatch(1)
  private[this] val comparator: Comparator[PollingWatchKey] = new Comparator[PollingWatchKey] {
    override def compare(left: PollingWatchKey, right: PollingWatchKey): Int =
      left.path.compareTo(right.path)
  }
  override def close(): Unit = if (closed.compareAndSet(false, true)) {
    thread.interrupt()
    if (shutdownLatch.await(5, TimeUnit.SECONDS)) thread.join()
    registered.clear()
    readyKeys.clear()
  }

  override def init(): Unit = {
    ensureNotClosed()
    thread.start()
  }

  override def poll(timeout: Duration): WatchKey = {
    ensureNotClosed()
    timeout match {
      case d: FiniteDuration if d > 0.seconds => readyKeys.poll(d.toNanos, TimeUnit.NANOSECONDS)
      case _: FiniteDuration                  => readyKeys.poll(0, TimeUnit.NANOSECONDS)
      case _                                  => readyKeys.take()
    }
  }

  override def pollEvents(): Map[WatchKey, immutable.Seq[WatchEvent[NioPath]]] = {
    ensureNotClosed()
    val events = readyKeys.synchronized {
      val e = new util.ArrayList[PollingWatchKey](readyKeys.size)
      readyKeys.drainTo(e)
      e
    }
    events.asScala.map(e => e -> e.pollEventsImpl.asScala.toVector).toMap
  }

  override def register(path: NioPath, events: WatchEvent.Kind[NioPath]*): WatchKey = {
    ensureNotClosed()
    val newKey = new PollingWatchKey(path, events: _*)
    registered.putIfAbsent(path, newKey) match {
      case Some(k) => k
      case None =>
        val timestamps = getTimestamps(path)
        lastTimestamps.synchronized(lastTimestamps ++= timestamps)
        newKey
    }
  }
  override def unregister(path: NioPath): Unit = {
    ensureNotClosed()
    registered.remove(path)
    ()
  }

  private def ensureNotClosed(): Unit =
    if (closed.get()) throw new ClosedWatchServiceException

  private def getTimestamps(path: NioPath): Seq[(NioPath, Long)] =
    view.list(path.toGlob, AllPass) ++ view.list(path * AllPassFilter, AllPass)

  private def getTimestamps: immutable.Seq[(NioPath, Long)] = {
    registered.keys.toIndexedSeq.flatMap(getTimestamps)
  }

  private class PollingThread extends Thread("sbt.io.watch.PollingThread") {
    def this(duration: FiniteDuration) = this()
    setDaemon(true)
    private[this] val latch = new CountDownLatch(1)
    override def start(): Unit = {
      super.start()
      if (!latch.await(5, TimeUnit.SECONDS))
        throw new IllegalStateException("Couldn't start polling thread!")
    }
    override def run(): Unit = {
      latch.countDown()
      def continue: Boolean = !closed.get() && !Thread.currentThread.isInterrupted
      def getKeyAndContext(path: NioPath): Option[(PollingWatchKey, NioPath)] =
        (registered.get(path.getParent) orElse registered.get(path)).map(k =>
          k -> k.path.relativize(path))
      @tailrec
      def updateFileTimes(): Unit = {
        if (continue) {
          val newState = getTimestamps
          val map = newState.toMap
          val oldState = lastTimestamps.synchronized {
            val res = lastTimestamps
            lastTimestamps = map
            res
          }
          val deleted = new ArrayBuffer[NioPath]()
          val updated = new ArrayBuffer[NioPath]()
          val created = new ArrayBuffer[NioPath]()
          newState.foreach {
            case (path, lastModified) =>
              oldState.get(path) match {
                case Some(lm) => if (lm != lastModified) updated += path
                case None     => created += path
              }
          }
          oldState.foreach { case (path, _) => if (!map.contains(path)) deleted += path }

          val keys = new util.HashSet[PollingWatchKey].asScala
          def addEvents(paths: ArrayBuffer[NioPath], kind: WatchEvent.Kind[NioPath]): Unit =
            paths.sorted.foreach { path =>
              getKeyAndContext(path).foreach {
                case (k, c) => keys ++= k.maybeAddEvent(new PollingWatchEvent(c, kind))
              }
            }
          addEvents(created, ENTRY_CREATE)
          addEvents(updated, ENTRY_MODIFY)
          addEvents(deleted, ENTRY_DELETE)
          val sorted = new util.ArrayList[PollingWatchKey](keys.asJava)
          Collections.sort(sorted, comparator)
          readyKeys.synchronized(readyKeys.addAll(sorted))
          if (continue) {
            val keepGoing = try {
              Thread.sleep(delay.toMillis)
              true
            } catch {
              case _: InterruptedException => false
            }
            if (keepGoing) updateFileTimes()
          }
        }
      }
      updateFileTimes()
      shutdownLatch.countDown()
    }

  }

  private object Overflow
      extends PollingWatchEvent(null, OVERFLOW.asInstanceOf[WatchEvent.Kind[NioPath]])
  private class PollingWatchKey(private[PollingWatchService] val path: NioPath,
                                eventKinds: WatchEvent.Kind[NioPath]*)
      extends WatchKey {
    private[this] val events = new ArrayBlockingQueue[WatchEvent[NioPath]](256)
    private[this] val hasOverflow = new AtomicBoolean(false)
    private[this] lazy val acceptCreate = eventKinds.contains(ENTRY_CREATE)
    private[this] lazy val acceptDelete = eventKinds.contains(ENTRY_DELETE)
    private[this] lazy val acceptModify = eventKinds.contains(ENTRY_MODIFY)
    override def cancel(): Unit = {
      reset()
      registered.remove(path)
      ()
    }
    override def isValid: Boolean = true
    override def pollEvents(): JList[WatchEvent[_]] =
      pollEventsImpl.asInstanceOf[JList[WatchEvent[_]]]
    override def reset(): Boolean = events.synchronized {
      events.clear()
      true
    }
    override def watchable(): Watchable = path

    private[PollingWatchService] def pollEventsImpl: JList[WatchEvent[NioPath]] =
      events.synchronized {
        val overflow = hasOverflow.getAndSet(false)
        val size = events.size + (if (overflow) 1 else 0)
        val res = new util.ArrayList[WatchEvent[NioPath]](size)
        if (overflow) res.add(Overflow)
        events.drainTo(res)
        res
      }
    private[PollingWatchService] def maybeAddEvent(
        event: WatchEvent[NioPath]): Option[PollingWatchKey] = {
      def offer(event: WatchEvent[NioPath]): Option[PollingWatchKey] = {
        if (!events.synchronized(events.offer(event))) hasOverflow.set(true)
        Some(this)
      }
      event.kind match {
        case ENTRY_CREATE if acceptCreate => offer(event)
        case ENTRY_DELETE if acceptDelete => offer(event)
        case ENTRY_MODIFY if acceptModify => offer(event)
        case _                            => None
      }
    }
  }

}

private class PollingWatchEvent(
    override val context: NioPath,
    override val kind: WatchEvent.Kind[NioPath]
) extends WatchEvent[NioPath] {
  override val count: Int = 1
}
