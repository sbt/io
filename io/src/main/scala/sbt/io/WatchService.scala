package sbt.io

import java.nio.file.{
  ClosedWatchServiceException,
  WatchEvent,
  WatchKey,
  Path => JPath,
  WatchService => JWatchService
}
import java.util.concurrent.TimeUnit

import scala.annotation.tailrec
import scala.collection.{ immutable, mutable }
import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration

object WatchService {

  /**
   * Adapts a Java `WatchService` to be used with sbt's `WatchService` infrastructure.
   * @param service The `WatchService` to use.
   */
  implicit final class WatchServiceAdapter(service: JWatchService)
      extends WatchService
      with Unregisterable {
    private var closed: Boolean = false
    private val registered: mutable.Map[JPath, WatchKey] = mutable.Map.empty

    override def init(): Unit =
      ()

    override def pollEvents(): Map[WatchKey, immutable.Seq[WatchEvent[JPath]]] = {
      val values = registered.synchronized(registered.values.toIndexedSeq)
      values.flatMap { k =>
        val events = k.pollEvents()
        if (events.isEmpty) None
        else Some((k, events.asScala.asInstanceOf[Seq[WatchEvent[JPath]]].toIndexedSeq))
      }.toMap
    }

    @tailrec
    override def poll(timeout: Duration): WatchKey =
      if (timeout.isFinite) {
        service.poll(timeout.toMillis, TimeUnit.MILLISECONDS)
      } else {
        service.poll(1000L, TimeUnit.MILLISECONDS) match {
          case null => poll(timeout)
          case key  => key
        }
      }

    override def register(path: JPath, events: WatchEvent.Kind[JPath]*): WatchKey = {
      if (closed) throw new ClosedWatchServiceException
      else {
        registered.synchronized {
          registered.get(path) match {
            case None =>
              val key = path.register(service, events: _*)
              registered += path -> key
              key
            case Some(key) =>
              key
          }
        }
      }
    }

    override def unregister(path: JPath): Unit = {
      if (closed) throw new ClosedWatchServiceException
      registered.synchronized {
        registered.get(path) match {
          case Some(key) =>
            key.cancel()
            registered -= path
          case _ =>
        }
      }
      ()
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
 * A service that will trigger the file system for file creation, deletion
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
  def pollEvents(): Map[WatchKey, immutable.Seq[WatchEvent[JPath]]]

  /**
   * Retrieves the next `WatchKey` that has a `WatchEvent` waiting. Waits
   * until the `timeout` is expired is no such key exists.
   * @param timeout Maximum time to wait
   * @return The next `WatchKey` that received an event, or null if no such
   *         key exists.
   */
  def poll(timeout: Duration): WatchKey

  /**
   * Registers a path to be monitored.
   * @param path The path to trigger.
   * @param events The events that should be registered.
   * @return A `WatchKey`, that represents a token of registration.
   */
  def register(path: JPath, events: WatchEvent.Kind[JPath]*): WatchKey

  /**
   * Closes this `WatchService`.
   */
  def close(): Unit
}

trait Unregisterable { self: WatchService =>

  /**
   * Unregisters a monitored path.
   * @param path The monitored path.
   */
  def unregister(path: JPath): Unit
}
