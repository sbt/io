package sbt.io

import java.nio.file.{ ClosedWatchServiceException, WatchEvent, WatchKey, Path => JPath, WatchService => JWatchService }
import java.util.concurrent.TimeUnit

import scala.collection.mutable

import scala.collection.JavaConverters._

object WatchService {

  /**
   * Adapts a Java `WatchService` to be used with sbt's `WatchService` infrastructure.
   * @param service The `WatchService` to use.
   */
  implicit final class WatchServiceAdapter(service: JWatchService) extends WatchService {
    private var closed: Boolean = false
    private val registered: mutable.Buffer[WatchKey] = mutable.Buffer.empty

    override def init(): Unit =
      ()

    override def pollEvents(): Map[WatchKey, Seq[WatchEvent[JPath]]] =
      registered.flatMap { k =>
        val events = k.pollEvents()
        if (events.isEmpty) None
        else Some((k, events.asScala.asInstanceOf[Seq[WatchEvent[JPath]]]))
      }.toMap

    override def poll(timeoutMs: Long): WatchKey = {
      service.poll(timeoutMs, TimeUnit.MILLISECONDS)
    }

    override def register(path: JPath, events: WatchEvent.Kind[JPath]*): WatchKey = {
      if (closed) throw new ClosedWatchServiceException
      else {
        val key = path.register(service, events: _*)
        registered += key
        key
      }
    }

    override def close(): Unit = {
      closed = true
      service.close()
    }

    override def toString(): String =
      service.toString()
  }

}

/**
 * A service that will monitor the file system for file creation, deletion
 * and modification.
 */
trait WatchService {

  /** Initializes the watchservice. */
  def init(): Unit

  /**
   * Retrieves all the events and groups them by watch key.
   * Does not wait if no event is available.
   * @return The pending events.
   */
  def pollEvents(): Map[WatchKey, Seq[WatchEvent[JPath]]]

  /**
   * Retrieves the next `WatchKey` that has a `WatchEvent` waiting. Waits
   * up to `timeoutMs` milliseconds if no such key exists.
   * @param timeoutMs Maximum time to wait, in milliseconds.
   * @return The next `WatchKey` that received an event.
   */
  def poll(timeoutMs: Long): WatchKey

  /**
   * Registers a path to be monitored.
   * @param path The path to monitor.
   * @param events The events that should be registered.
   * @return A `WatchKey`, that represents a token of registration.
   */
  def register(path: JPath, events: WatchEvent.Kind[JPath]*): WatchKey

  /**
   * Closes this `WatchService`.
   */
  def close(): Unit
}
