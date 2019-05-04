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

import java.nio.file.StandardWatchEventKinds._
import java.nio.file.{ WatchService => _, _ }
import java.util
import java.util.concurrent._
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }
import java.util.{ List => JList }

import sbt.internal.nio.FileEvent.{ Creation, Deletion, Update }
import sbt.io._
import sbt.nio.file.{ AnyPath, Glob }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.concurrent.duration.{ Deadline => _, _ }
import scala.util.Random

/** A `WatchService` that polls the filesystem every `delay`. */
private[sbt] class PollingWatchService(delay: FiniteDuration, timeSource: TimeSource)
    extends WatchService
    with Unregisterable {
  def this(delay: FiniteDuration) = this(delay, TimeSource.default)
  private[this] implicit def ts: TimeSource = timeSource
  private[this] val closed = new AtomicBoolean(false)
  private[this] val registered = new ConcurrentHashMap[Path, PollingWatchKey].asScala
  private[this] val lastModifiedConverter: Path => Long = p => IO.getModifiedTimeOrZero(p.toFile)
  private[this] val pollQueue: util.Queue[PollingWatchKey] =
    new LinkedBlockingDeque[PollingWatchKey]
  private[this] val random = new Random()
  override def close(): Unit = if (closed.compareAndSet(false, true)) {
    registered.clear()
  }

  override def init(): Unit = {
    ensureNotClosed()
  }

  override def poll(timeout: Duration): WatchKey = {
    ensureNotClosed()
    val numKeys = pollQueue.size
    val (adjustedTimeout, deadline) = timeout match {
      case t: FiniteDuration => t -> (Deadline.now + t)
      case _                 => (numKeys * 2.millis) -> Deadline.Inf
    }
    val millis = adjustedTimeout.toMillis
    val (batchSize, batchTimeout) = (1 to Int.MaxValue)
      .dropWhile(i => (millis * i) / numKeys == 0)
      .headOption
      .fold(1 -> 1.millis)(i => i -> (adjustedTimeout / i.toLong))
    pollImpl(batchSize, batchTimeout, deadline).orNull
  }

  @tailrec
  private def pollImpl(
      batchSize: Int,
      duration: FiniteDuration,
      deadline: Deadline
  ): Option[WatchKey] = {
    pollQueue.poll() match {
      case null => None
      case key =>
        pollQueue.add(key)
        key.poll() match {
          case r if r.isDefined => r
          case _ =>
            if (batchSize > 1) {
              pollImpl(batchSize - 1, duration, deadline)
            } else if (!deadline.isOverdue) {
              Thread.sleep(duration.toMillis)
              pollImpl(batchSize, duration, deadline)
            } else {
              None
            }
        }
    }
  }

  override def pollEvents(): Map[WatchKey, immutable.Seq[WatchEvent[Path]]] = {
    ensureNotClosed()
    registered.values.map(k => (k: WatchKey) -> k.pollEventsImpl.asScala.toVector).toMap
  }

  override def register(path: Path, events: WatchEvent.Kind[Path]*): WatchKey = {
    ensureNotClosed()
    registered.get(path) match {
      case Some(k) => k
      case None =>
        val newKey = new PollingWatchKey(path, events: _*)
        registered.put(path, newKey)
        pollQueue.add(newKey)
        newKey
    }
  }
  override def unregister(path: Path): Unit = {
    ensureNotClosed()
    registered.remove(path)
    pollQueue.removeIf(_.path == path)
    ()
  }

  private def ensureNotClosed(): Unit =
    if (closed.get()) throw new ClosedWatchServiceException

  private object Overflow
      extends PollingWatchEvent(null, OVERFLOW.asInstanceOf[WatchEvent.Kind[Path]])
  private class PollingWatchKey(
      private[PollingWatchService] val path: Path,
      eventKinds: WatchEvent.Kind[Path]*
  ) extends WatchKey {
    private[this] val events =
      new ArrayBlockingQueue[FileEvent[Long]](256)
    private[this] val hasOverflow = new AtomicBoolean(false)
    private[this] lazy val acceptCreate = eventKinds.contains(ENTRY_CREATE)
    private[this] lazy val acceptDelete = eventKinds.contains(ENTRY_DELETE)
    private[this] lazy val acceptModify = eventKinds.contains(ENTRY_MODIFY)
    private[this] val glob = Glob(path, AnyPath)
    private[this] val fileCache =
      new FileCache[Long](lastModifiedConverter)
    fileCache.register(glob)
    private[this] def nextPollTime: Deadline =
      Deadline.now + random.nextInt(2 * delay.toMillis.toInt).millis
    private[this] val lastPolled = new AtomicReference(nextPollTime)
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

    private[PollingWatchService] def poll(): Option[WatchKey] = {
      lastPolled.get match {
        case d if d < Deadline.now =>
          val res = fileCache.refresh(glob)
          lastPolled.set(Deadline.now)
          res.foreach(maybeAddEvent)
          if (events.isEmpty) None else Some(this)
        case _ => None
      }
    }
    private[PollingWatchService] def pollEventsImpl: JList[WatchEvent[Path]] = {
      events.synchronized {
        val overflow = hasOverflow.getAndSet(false)
        val size = events.size + (if (overflow) 1 else 0)
        val rawEvents = new util.ArrayList[FileEvent[Long]](size)
        events.drainTo(rawEvents)
        val res = new util.ArrayList[WatchEvent[Path]](size)
        res.addAll(rawEvents.asScala.map {
          case Creation(p, _)  => new PollingWatchEvent(p, ENTRY_CREATE)
          case Deletion(p, _)  => new PollingWatchEvent(p, ENTRY_DELETE)
          case Update(p, _, _) => new PollingWatchEvent(p, ENTRY_MODIFY)
        }.asJava)
        if (overflow) res.add(Overflow)
        res
      }
    }
    private[PollingWatchService] def maybeAddEvent(
        event: FileEvent[Long]
    ): Option[PollingWatchKey] = {
      def offer(event: FileEvent[Long]): Option[PollingWatchKey] = {
        if (!events.synchronized(events.offer(event))) hasOverflow.set(true)
        Some(this)
      }
      event match {
        case _: Creation[_] if acceptCreate => offer(event)
        case _: Deletion[_] if acceptDelete => offer(event)
        case _: Update[_] if acceptModify   => offer(event)
        case _                              => None
      }
    }
  }

}

private class PollingWatchEvent(
    override val context: Path,
    override val kind: WatchEvent.Kind[Path]
) extends WatchEvent[Path] {
  override val count: Int = 1
  override def toString: String = kind match {
    case ENTRY_CREATE => s"Creation($context)"
    case ENTRY_DELETE => s"Deletion($context)"
    case ENTRY_MODIFY => s"Modify($context)"
    case _            => "Overflow"
  }
}
