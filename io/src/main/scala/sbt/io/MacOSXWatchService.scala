package sbt.io

import java.io.IOException
import java.nio.file.{ WatchEvent, WatchKey, Path => JPath }
import java.util.Collections
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ ConcurrentHashMap, TimeUnit }

import scala.collection.JavaConverters._
import scala.collection.{ immutable, mutable }
import scala.concurrent.duration._

class MacOSXWatchService extends WatchService with Unregisterable {
  private val underlying = com.swoval.files.RegisterableWatchServices.get()
  private val keys: mutable.Map[JPath, WatchKey] =
    Collections.synchronizedMap(new ConcurrentHashMap[JPath, WatchKey]()).asScala
  private val isClosed = new AtomicBoolean(false)
  def isOpen: Boolean = !isClosed.get

  override def init(): Unit = {}

  override def pollEvents(): Map[WatchKey, immutable.Seq[WatchEvent[JPath]]] =
    underlying.poll() match {
      case null => Map.empty
      case k =>
        Map(k -> k.pollEvents().asScala.view.map(_.asInstanceOf[WatchEvent[JPath]]).toIndexedSeq)
    }

  override def poll(timeout: Duration): WatchKey =
    underlying.poll(timeout.toNanos, TimeUnit.NANOSECONDS)

  override def register(path: JPath, events: WatchEvent.Kind[JPath]*): WatchKey = {
    val resolved = resolve(path)
    val key = underlying.register(resolved, events: _*)
    keys.put(resolved, key)
    key
  }

  override def unregister(path: JPath): Unit = {
    keys.remove(resolve(path)) foreach (_.cancel())
  }

  override def close(): Unit = if (isClosed.compareAndSet(false, true)) {
    keys.values.foreach(_.cancel())
    keys.clear()
    underlying.close()
  }

  private def resolve(path: JPath): JPath =
    try path.toRealPath()
    catch { case _: IOException => if (path.isAbsolute) path else path.toAbsolutePath }
}
