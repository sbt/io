package sbt.internal.io
import java.util
import java.util.concurrent.atomic.AtomicInteger

import sbt.io.FileTreeDataView.{ Entry, Observable, Observer }

import scala.collection.mutable
import scala.collection.JavaConverters._

private[io] class Observers[T] extends Observer[T] with Observable[T] {
  private[this] val id = new AtomicInteger(0)
  private[this] val observers: mutable.Map[Int, Observer[T]] =
    new util.LinkedHashMap[Int, Observer[T]]().asScala

  override def onCreate(newEntry: Entry[T]): Unit = observers.synchronized {
    observers.values.foreach(_.onCreate(newEntry))
  }

  override def onDelete(oldEntry: Entry[T]): Unit = observers.synchronized {
    observers.values.foreach(_.onDelete(oldEntry))
  }

  override def onUpdate(oldEntry: Entry[T], newEntry: Entry[T]): Unit = observers.synchronized {
    observers.values.foreach(_.onUpdate(oldEntry, newEntry))
  }

  override def addObserver(observer: Observer[T]) = observers.synchronized {
    val observerId = id.incrementAndGet()
    observers += observerId -> observer
    observerId
  }

  override def removeObserver(handle: Int): Unit = observers.synchronized {
    observers -= handle
    ()
  }

  override def close(): Unit = observers.synchronized(observers.clear())
}
