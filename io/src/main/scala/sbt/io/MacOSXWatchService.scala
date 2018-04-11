package sbt.io

import java.nio.file.StandardWatchEventKinds.{ ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY, OVERFLOW }
import java.nio.file.{
  ClosedWatchServiceException,
  Files,
  WatchEvent,
  WatchKey,
  Path => JPath,
  Paths => JPaths
}
import java.util.Collections
import java.util.concurrent._
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }
import java.util.{ List => JList }

import com.swoval.concurrent.ThreadFactory
import com.swoval.files.apple.FileEventsApi.Consumer
import com.swoval.files.apple.{ FileEvent, FileEventsApi, Flags }

import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.concurrent.duration._

class MacOSXWatchService extends WatchService {
  // The FsEvents api doesn't seem to report events at lower than 10 millisecond intervals.
  private[this] val watchLatency: Duration = 10.milliseconds
  private[this] val queueSize = 256

  private[this] val executor =
    Executors.newSingleThreadExecutor(new ThreadFactory("sbt.io.MacOSXWatchService"))
  private[this] def async[R](f: => R): Unit = {
    executor.submit(new Runnable() { override def run(): Unit = { f; () } })
    ()
  }
  private[this] val dropEvent = new Consumer[String] {
    override def accept(s: String): Unit = {}
  }
  private val onFileEvent = new Consumer[FileEvent] {
    override def accept(fileEvent: FileEvent): Unit = async {
      val path = JPaths.get(fileEvent.fileName)
      registered.synchronized(registered.get(path) orElse registered.get(path.getParent) foreach {
        case (key, _) =>
          val exists = Files.exists(path)
          if (exists && key.reportModifyEvents) createEvent(key, ENTRY_MODIFY, path)
          else if (!exists && key.reportDeleteEvents) createEvent(key, ENTRY_DELETE, path)
      })
    }
  }

  private[this] val watcher: FileEventsApi = FileEventsApi.apply(onFileEvent, dropEvent)
  override def close(): Unit = watcher.synchronized {
    if (open.compareAndSet(true, false)) {
      watcher.close()
      executor.shutdownNow()
      executor.awaitTermination(5, TimeUnit.SECONDS)
      ()
    } else {}
  }

  override def init(): Unit = {}

  override def poll(timeout: Duration): WatchKey =
    if (isOpen) {
      readyKeys.poll(timeout.toNanos, TimeUnit.NANOSECONDS)
    } else throw new ClosedWatchServiceException

  override def pollEvents(): Map[WatchKey, Seq[WatchEvent[JPath]]] =
    registered
      .synchronized(registered.flatMap {
        case (_, k) =>
          val events = k.pollEvents()
          if (events.isEmpty) None
          else Some(k -> events.asScala.map(_.asInstanceOf[WatchEvent[JPath]]))
      })
      .toMap[WatchKey, Seq[WatchEvent[JPath]]]

  override def register(path: JPath, events: WatchEvent.Kind[JPath]*): WatchKey = {
    if (isOpen) {
      registered.synchronized {
        val realPath = path.toRealPath()
        registered get realPath match {
          case Some(k) => k;
          case _ =>
            val key = new MacOSXWatchKey(realPath, queueSize, events: _*)
            registered += realPath -> key
            val flags = new Flags.Create().setNoDefer().setFileEvents().value
            watcher.createStream(realPath.toString, watchLatency.toMillis / 1000.0, flags)
            key
        }
      }
    } else throw new ClosedWatchServiceException
  }

  private def createEvent(key: MacOSXWatchKey, kind: WatchEvent.Kind[JPath], file: JPath): Unit = {
    val event = Event(kind, 1, file)
    key.addEvent(event)
    if (!readyKeys.contains(key)) {
      readyKeys.offer(key)
    }
    ()
  }

  def isOpen: Boolean = open.get

  private[this] val open = new AtomicBoolean(true)
  private[this] val readyKeys = new LinkedBlockingQueue[MacOSXWatchKey]
  private[this] val registered = mutable.Map.empty[JPath, MacOSXWatchKey]
}

private case class Event[T](kind: WatchEvent.Kind[T], count: Int, context: T) extends WatchEvent[T]

private class MacOSXWatchKey(val watchable: JPath, queueSize: Int, kinds: WatchEvent.Kind[JPath]*)
    extends WatchKey {

  override def cancel(): Unit = valid.set(false)

  override def isValid: Boolean = valid.get

  override def pollEvents(): JList[WatchEvent[_]] = {
    val result = new mutable.ArrayBuffer[WatchEvent[_]](events.size).asJava
    events.drainTo(result)
    val overflowCount = overflow.getAndSet(0)
    if (overflowCount != 0) {
      result.add(Event(OVERFLOW, overflowCount, watchable))
    }
    Collections.unmodifiableList(result)
  }

  override def reset(): Boolean = { events.clear(); true }

  override def toString = s"MacOSXWatchKey($watchable)"

  lazy val reportCreateEvents: Boolean = kinds contains ENTRY_CREATE
  lazy val reportModifyEvents: Boolean = kinds contains ENTRY_MODIFY
  lazy val reportDeleteEvents: Boolean = kinds contains ENTRY_DELETE

  private val events = new ArrayBlockingQueue[WatchEvent[_]](queueSize)
  private val overflow = new AtomicInteger()
  private val valid = new AtomicBoolean(true)

  @inline def addEvent(event: Event[JPath]): Unit = if (!events.offer(event)) {
    overflow.incrementAndGet()
    ()
  }
}
