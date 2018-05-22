package sbt.io

import java.nio.file.StandardWatchEventKinds._
import java.nio.file.{
  ClosedWatchServiceException,
  Files,
  WatchEvent,
  WatchKey,
  Watchable,
  Path => JPath
}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{ List => JList }

import sbt.io.syntax._

import scala.collection.{ immutable, mutable }
import scala.concurrent.duration.{ Duration, FiniteDuration }

/** A `WatchService` that polls the filesystem every `delay`. */
class PollingWatchService(delay: FiniteDuration) extends WatchService with Unregisterable {
  private var closed: Boolean = false
  private val thread: PollingThread = new PollingThread(delay)
  private val keys: mutable.Map[JPath, PollingWatchKey] = mutable.Map.empty
  private val pathLengthOrdering: Ordering[JPath] =
    Ordering.fromLessThan {
      case (null, _) | (_, null) => true
      case (a, b) =>
        a.toString.length < b.toString.length
    }

  private val watched: mutable.Map[JPath, Seq[WatchEvent.Kind[JPath]]] =
    mutable.Map.empty

  override def close(): Unit =
    closed = true

  override def init(): Unit = {
    ensureNotClosed()
    thread.start()
  }

  override def poll(timeout: Duration): WatchKey = thread.withKeys { keys =>
    ensureNotClosed()
    if (keys.isEmpty) {
      keys.wait(timeout.toMillis)
    }
    keys.headOption.map { k =>
      keys -= k
      k
    }.orNull
  }

  override def pollEvents(): Map[WatchKey, immutable.Seq[WatchEvent[JPath]]] =
    thread.withKeys { keys =>
      import scala.collection.JavaConverters._
      ensureNotClosed()
      val events =
        keys.map(k => k -> k.pollEvents().asScala.asInstanceOf[Seq[WatchEvent[JPath]]].toIndexedSeq)
      keys.clear()
      events.toMap
    }

  override def register(path: JPath, events: WatchEvent.Kind[JPath]*): WatchKey = {
    ensureNotClosed()
    val key = new PollingWatchKey(path, new java.util.ArrayList[WatchEvent[_]])
    keys += path -> key
    thread.setFileTimes(path)
    watched += path -> events
    key
  }

  override def unregister(path: JPath): Unit = {
    ensureNotClosed()
    watched -= path
    ()
  }

  private def ensureNotClosed(): Unit =
    if (closed) throw new ClosedWatchServiceException

  private class PollingThread(delay: FiniteDuration) extends Thread {
    private[this] val _keysWithEvents = mutable.LinkedHashSet.empty[WatchKey]
    private[this] val _initDone = new AtomicBoolean(false)
    private[this] var fileTimes: Map[JPath, Long] = Map.empty

    private[PollingWatchService] def withKeys[R](f: mutable.LinkedHashSet[WatchKey] => R): R =
      _keysWithEvents.synchronized(f(_keysWithEvents))

    @deprecated("The initDone variable should not be accessed externally", "1.1.17")
    def initDone: Boolean = _initDone.get()
    @deprecated("The initDone variable should not be set externally", "1.1.17")
    def initDone_=(initDone: Boolean) = _initDone.set(initDone)
    @deprecated("Use withKeys instead of directly accessing keysWithEvents", "1.1.17")
    def keysWithEvents: mutable.LinkedHashSet[WatchKey] = _keysWithEvents

    override def run(): Unit =
      while (!closed) {
        populateEvents()
        _initDone.synchronized {
          _initDone.set(true)
          _initDone.notify()
        }
        Thread.sleep(delay.toMillis)
      }
    override def start(): Unit = {
      super.start()
      _initDone.synchronized { while (!_initDone.get()) _initDone.wait() }
    }
    private[PollingWatchService] def setFileTimes(path: JPath): Unit = {
      val entries = path.toFile.allPaths.get.map(f => f.toPath -> IO.getModifiedTimeOrZero(f))
      fileTimes.synchronized(fileTimes ++= entries)
    }
    def getFileTimes(): Map[JPath, Long] = {
      val results = mutable.Map.empty[JPath, Long]
      watched.toSeq.sortBy(_._1)(pathLengthOrdering).foreach {
        case (p, _) =>
          if (!results.contains(p))
            p.toFile.allPaths.get.foreach(f => results += f.toPath -> IO.getModifiedTimeOrZero(f))
      }
      results.toMap
    }

    private def addEvent(path: JPath, ev: WatchEvent[JPath]): Unit = _keysWithEvents.synchronized {
      keys.get(path).foreach { k =>
        _keysWithEvents += k
        k.events.add(ev)
        _keysWithEvents.notifyAll()
      }
    }

    private def populateEvents(): Unit = {
      val (deletedFiles, createdFiles, modifiedFiles) = fileTimes.synchronized {
        val newFileTimes = getFileTimes()
        val newFiles = newFileTimes.keySet
        val oldFiles = fileTimes.keySet

        val deletedFiles = (oldFiles -- newFiles).toSeq
        val createdFiles = (newFiles -- oldFiles).toSeq

        val modifiedFiles = fileTimes.collect {
          case (p, oldTime) if newFileTimes.getOrElse(p, 0L) > oldTime => p
        }
        fileTimes = newFileTimes
        (deletedFiles, createdFiles, modifiedFiles)
      }

      deletedFiles
        .map { deleted =>
          val parent = deleted.getParent
          if (watched.getOrElse(parent, Seq.empty).contains(ENTRY_DELETE)) {
            val ev = new PollingWatchEvent(parent.relativize(deleted), ENTRY_DELETE)
            addEvent(parent, ev)
          }
          deleted
        }
        .foreach(watched -= _)

      createdFiles.sorted(pathLengthOrdering).foreach {
        case dir if Files.isDirectory(dir) =>
          val parent = dir.getParent
          val parentEvents = watched.getOrElse(parent, Seq.empty)
          if (parentEvents.contains(ENTRY_CREATE)) {
            val ev = new PollingWatchEvent(parent.relativize(dir), ENTRY_CREATE)
            addEvent(parent, ev)
          }

        case file =>
          val parent = file.getParent
          if (watched.getOrElse(parent, Seq.empty).contains(ENTRY_CREATE)) {
            val ev = new PollingWatchEvent(parent.relativize(file), ENTRY_CREATE)
            addEvent(parent, ev)
          }
      }

      modifiedFiles.foreach {
        case file =>
          val parent = file.getParent
          if (watched.getOrElse(parent, Seq.empty).contains(ENTRY_MODIFY)) {
            val ev = new PollingWatchEvent(parent.relativize(file), ENTRY_MODIFY)
            addEvent(parent, ev)
          }
      }
    }

  }

  private class PollingWatchKey(
      override val watchable: Watchable,
      val events: JList[WatchEvent[_]]
  ) extends WatchKey {
    override def cancel(): Unit = ()
    override def isValid(): Boolean = true
    override def pollEvents(): java.util.List[WatchEvent[_]] = {
      val evs = new java.util.ArrayList[WatchEvent[_]](events)
      events.clear()
      evs
    }
    override def reset(): Boolean = true
  }

}

private class PollingWatchEvent(
    override val context: JPath,
    override val kind: WatchEvent.Kind[JPath]
) extends WatchEvent[JPath] {
  override val count: Int = 1
}
