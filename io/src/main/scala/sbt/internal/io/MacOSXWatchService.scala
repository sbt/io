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
import java.nio.file.{ ClosedWatchServiceException, WatchEvent, WatchKey, Path => JPath }
import java.util.Collections
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ ConcurrentHashMap, TimeUnit }

import sbt.io.{ Unregisterable, WatchService }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.{ immutable, mutable }
import scala.concurrent.duration._

private[sbt] class MacOSXWatchService extends WatchService with Unregisterable {
  private val underlying = com.swoval.files.RegisterableWatchServices.get()
  private val keys: mutable.Map[JPath, WatchKey] =
    Collections.synchronizedMap(new ConcurrentHashMap[JPath, WatchKey]()).asScala
  private val parentKeys: mutable.Map[JPath, WatchKey] =
    Collections.synchronizedMap(new ConcurrentHashMap[JPath, WatchKey]()).asScala
  private val isClosed = new AtomicBoolean(false)
  def isOpen: Boolean = !isClosed.get

  override def init(): Unit = {}

  override def pollEvents(): Map[WatchKey, immutable.Seq[WatchEvent[JPath]]] = {
    underlying.poll() match {
      case null => Map.empty
      case k =>
        k.watchable() match {
          case p: JPath if keys.contains(p) =>
            val res = k -> k
              .pollEvents()
              .asScala
              .view
              .map(_.asInstanceOf[WatchEvent[JPath]])
              .toIndexedSeq
            Map(res)
          case _ => null
        }
    }
  }

  override def poll(timeout: Duration): WatchKey = {
    val finiteDuration: FiniteDuration = timeout match {
      case d: FiniteDuration => d
      case _                 => new FiniteDuration(Int.MaxValue, SECONDS)
    }
    val limit = finiteDuration.fromNow
    @tailrec def impl(): WatchKey = {
      val remaining = limit - Deadline.now
      if (remaining > 0.seconds) {
        underlying.poll((limit - Deadline.now).toNanos, TimeUnit.NANOSECONDS) match {
          case null => null
          case k =>
            k.watchable match {
              case p: JPath if keys.contains(p) => k
              case _                            => impl()
            }
        }
      } else null
    }
    impl()
  }

  override def register(path: JPath, events: WatchEvent.Kind[JPath]*): WatchKey =
    if (!isClosed.get()) {
      keys.get(path) match {
        case Some(k) => k
        case _ =>
          val resolved = resolve(path)
          val parent = path.getParent
          if (!keys.contains(parent)) {
            // workaround for https://github.com/sbt/sbt/issues/4603
            if (keys.keys.exists(
                  p =>
                    p.getParent == parent && {
                      val leftFileName = p.getFileName.toString
                      val rightFileName = path.getFileName.toString
                      leftFileName != rightFileName && (leftFileName
                        .startsWith(rightFileName) || rightFileName.startsWith(leftFileName))
                    }
                )) {
              parentKeys.put(parent, underlying.register(parent, events: _*))
            }
          }
          val key =
            parentKeys.remove(resolved) match {
              case Some(k) => k
              case _ =>
                underlying.register(resolved, events: _*)
            }
          keys.put(resolved, key)
          key
      }
    } else {
      throw new ClosedWatchServiceException
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
