package sbt.io

import java.nio.file.{ Path => JPath }
import java.util.concurrent.{ ArrayBlockingQueue, ConcurrentHashMap, TimeUnit }

import sbt.io.FileTreeDataView.Entry.EntryImpl
import sbt.io.FileTreeDataView.{ Entry, Observable, Observer }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration._

/**
 * Provides a blocking interface for awaiting events from an [[Observable]].
 * @tparam T the type of [[Entry.value]] instances
 */
trait FileEventMonitor[+T] extends AutoCloseable {

  /**
   * Block for the specified duration until an event is emitted or a timeout occurs.
   * @param duration the timeout (can be infinite)
   * @return a sequence of [[FileEventMonitor.Event]] instances.
   */
  def poll(duration: Duration): Seq[FileEventMonitor.Event[T]]
}
object FileEventMonitor {
  sealed trait Event[+T] {
    def entry: Entry[T]
    def occurredAt: Deadline
  }
  object Event {
    def unapply[T](event: Event[T]): Option[(Entry[T], Deadline)] =
      Some((event.entry, event.occurredAt))
  }

  final class Creation[+T](override val entry: Entry[T],
                           override val occurredAt: Deadline = Deadline.now)
      extends EntryImpl(entry, entry.value)
      with Event[T] {
    override def equals(o: Any): Boolean = o match {
      case that: Creation[_] => this.value == that.value
      case _                 => false
    }
    override def hashCode(): Int = entry.hashCode()
    override def toString: String = s"Creation(${entry.getPath})"
  }
  object Creation {
    def unapply[T](creation: Creation[T]): Option[(Entry[T], Deadline)] =
      Some((creation.entry, creation.occurredAt))
  }

  final class Update[+T](val previous: Entry[T],
                         override val entry: Entry[T],
                         override val occurredAt: Deadline = Deadline.now)
      extends EntryImpl(entry, entry.value)
      with Event[T] {
    override def equals(o: Any): Boolean = o match {
      case that: Update[_] =>
        this.previous == that.previous && this.entry == that.entry
      case _ => false
    }
    override def hashCode(): Int = previous.hashCode() ^ entry.hashCode()
    override def toString: String = s"Update(${entry.getPath})"
  }
  object Update {
    def unapply[T](update: Update[T]): Option[(Entry[T], Entry[T], Deadline)] =
      Some((update.previous, update.entry, update.occurredAt))
  }

  final class Deletion[+T](val entry: Entry[T], override val occurredAt: Deadline = Deadline.now)
      extends EntryImpl(entry, entry.value)
      with Event[T] {
    override def equals(o: Any): Boolean = o match {
      case that: Deletion[_] => this.value == that.value
      case _                 => false
    }
    override def hashCode(): Int = entry.hashCode()
    override def toString: String = s"Deletion(${entry.getPath})"
  }
  object Deletion {
    def unapply[T](deletion: Deletion[T]): Option[(Entry[T], Deadline)] =
      Some((deletion.entry, deletion.occurredAt))
  }

  def apply[T](observable: Observable[T], logger: Logger = NullLogger): FileEventMonitor[T] =
    new FileEventMonitorImpl[T](observable, logger)
  def antiEntropy[T](observable: Observable[T],
                     period: FiniteDuration,
                     logger: Logger,
                     quarantinePeriod: FiniteDuration = 50.millis): FileEventMonitor[T] = {
    new AntiEntropyFileEventMonitor(period,
                                    new FileEventMonitorImpl[T](observable, logger),
                                    logger,
                                    quarantinePeriod)
  }

  private class FileEventMonitorImpl[T](observable: Observable[T], logger: Logger)
      extends FileEventMonitor[T] {
    private val events =
      new ConcurrentHashMap[JPath, FileEventMonitor.Event[T]]().asScala
    private val queue = new ArrayBlockingQueue[Unit](1)
    /*
     * This method will coalesce the new event with a possibly existing previous event. The aim is
     * that whenever the user calls poll, they will get the final difference between the previous
     * state of the file system and the new state, but without the incremental changes that may
     * have occurred along the way.
     */
    private def add(event: Event[T]): Unit = {
      logger.debug(s"Received $event")
      val path = event.entry.getPath
      events.putIfAbsent(path, event) match {
        case Some(d: Deletion[T]) =>
          event match {
            case _: Deletion[T] =>
            case Update(previous, _, _) =>
              events.put(path, new Deletion(previous, event.occurredAt))
            case _ => events.put(path, new Update(d.entry, event.entry, event.occurredAt))
          }
        case Some(_: Creation[T]) =>
          event match {
            case _: Deletion[T] => events.remove(path)
            case _: Update[T]   => events.put(path, new Creation(event.entry, event.occurredAt))
            case _              => events.put(path, event)
          }
        case Some(Update(previous, _, ts)) =>
          event match {
            case _: Deletion[T] => events.put(path, new Deletion(previous, ts))
            case e              => events.put(path, new Update(previous, e.entry, ts))
          }
        case None =>
      }
      queue.offer(())
      ()
    }
    private val handle = observable.addObserver(
      Observer(
        (entry: Entry[T]) => add(new Creation(entry)),
        (entry: Entry[T]) => add(new Deletion(entry)),
        (previous: Entry[T], current: Entry[T]) => add(new Update(previous, current))
      ))

    override def poll(duration: Duration): Seq[FileEventMonitor.Event[T]] = {
      duration match {
        case d: FiniteDuration => queue.poll(d.toMillis, TimeUnit.MILLISECONDS)
        case _                 => queue.take()
      }
      val res = events.values.toVector
      events.clear()
      res
    }

    override def close(): Unit = {
      observable.removeObserver(handle)
      events.clear()
      observable.close()
    }
  }
  private class AntiEntropyFileEventMonitor[T](period: FiniteDuration,
                                               fileEventMonitor: FileEventMonitor[T],
                                               logger: Logger,
                                               quarantinePeriod: FiniteDuration)
      extends FileEventMonitor[T] {
    private[this] val recentEvents = mutable.Map.empty[JPath, Deadline]
    /*
     * It is very common for file writes to be implemented as a move, which manifests as a delete
     * followed by a write. In sbt, this can manifest as continuous builds triggering for the delete
     * and re-compiling before the replaced file is present. This is undesirable because deleting
     * the file can break the build. Even if it doesn't break the build, it may cause the build to
     * become inconsistent with the file system if the creation is dropped due to anti-entropy. To
     * avoid this behavior, we quarantine the deletion and return immediately if a subsequent
     * creation is detected. This provides a reasonable compromise between low latency and
     * correctness.
     */
    private[this] val quarantinedEvents = mutable.Map.empty[JPath, Event[T]]
    @tailrec
    override final def poll(duration: Duration): Seq[Event[T]] = {
      val start = Deadline.now
      /*
       * The impl is tail recursive to handle the case when we quarantine a deleted file or find
       * an event for a path that is an anti-entropy quarantine. In these cases, if there are other
       * events in the queue, we want to immediately pull them. Otherwise it's possible to return
       * None while there events ready in the queue.
       */
      val results = fileEventMonitor.poll(duration)
      /*
       * Note that this transformation is not purely functional because it has the side effect of
       * modifying the quarantinedEvents and recentEvents maps.
       */
      val transformed = results.flatMap {
        case event @ Event(entry, occurredAt) =>
          val quarantined = if (entry.exists) quarantinedEvents.remove(entry.getPath) else None
          quarantined match {
            case Some(Deletion(deletedEntry, deletionTs)) =>
              recentEvents.put(entry.getPath, deletionTs + period)
              logger.debug(
                s"Triggering event for newly created path ${entry.getPath} that was previously quarantined.")
              Some(new Update(deletedEntry, entry, deletionTs))
            case _ =>
              recentEvents.get(entry.getPath) match {
                case Some(deadline) if (deadline - occurredAt) < period =>
                  logger.debug(s"Discarding entry for recently updated path ${entry.getPath}")
                  None
                case _ if !entry.exists =>
                  quarantinedEvents.put(entry.getPath, event)
                  logger.debug(s"Quarantining deletion event for path ${entry.getPath} for $period")
                  None
                case _ =>
                  recentEvents.put(entry.getPath, occurredAt + period)
                  logger.debug(s"Received event for path ${entry.getPath}")
                  Some(event)
              }
          }
      } ++ quarantinedEvents.collect {
        case (path, event @ Deletion(_, deadline)) if (deadline + quarantinePeriod).isOverdue =>
          quarantinedEvents.remove(path)
          recentEvents.put(path, deadline + period)
          logger.debug(s"Triggering event for previously quarantined deleted file: $path")
          event
      }
      recentEvents.retain((_, deadline) => !deadline.isOverdue)
      transformed match {
        case s if s.nonEmpty => s
        case _ =>
          val limit = duration - (Deadline.now - start)
          if (limit > 0.millis) poll(limit) else Nil
      }
    }

    override def close(): Unit = {
      quarantinedEvents.clear()
      recentEvents.clear()
      fileEventMonitor.close()
    }
  }
}
