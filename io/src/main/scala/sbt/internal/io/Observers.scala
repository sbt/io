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
import java.nio.file.{ Path => NioPath }
import java.util
import java.util.concurrent.atomic.AtomicInteger

import sbt.internal.io.FileEvent.{ Deletion, Update }
import sbt.io.Glob

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.control.NonFatal

private[sbt] trait Observer[-T] extends AutoCloseable {

  /**
   * Run a callback
   * @param t the generic type of events
   */
  def onNext(t: T): Unit

  /**
   * This is for managed observers that may need to be removed once the caller is done with them.
   * See [[sbt.internal.io.Observers]].
   */
  override def close(): Unit = {}
}

/**
 * An object that monitors a file system. The interface is very similar to that provided by other
 * libraries/frameworks, such as [[http://reactivex.io/intro.html rxJava]]. When it detects changes
 * in the file system, it will invoke a set of user specified callbacks. The Observable also
 * allows the user to add and removes paths to monitor.
 *
 * @tparam T the generic type of observable values
 */
private[sbt] trait Observable[+T] extends AutoCloseable {

  /**
   * Add callbacks to be invoked on file events.
   *
   * @param observer the callbacks to invoke
   * @return a handle to the callback.
   */
  def addObserver(observer: Observer[T]): AutoCloseable
}

private[sbt] trait ObservablePaths[+T] extends Observable[(NioPath, T)]

private[sbt] object Observable {
  def map[T, R](observable: Observable[T], f: T => R): Observable[R] = new Observable[R] {
    override def addObserver(observer: Observer[R]): AutoCloseable =
      observable.addObserver(new Observer[T] {
        override def onNext(t: T): Unit = observer.onNext(f(t))
      })
    override def close(): Unit = observable.close()
  }
  def filter[T](observable: Observable[T], f: T => Boolean): Observable[T] = new Observable[T] {
    override def addObserver(observer: Observer[T]): AutoCloseable =
      observable.addObserver(new Observer[T] {
        override def onNext(t: T): Unit = if (f(t)) observer.onNext(t)
      })
    override def close(): Unit = observable.close()
  }
}

/**
 * Aggregates a collection of [[Observer]]s into a single [[Observer]]. The callbacks for the
 * generated [[Observer]] invoke the corresponding callback for each of the [[Observer]]s that
 * are added via [[addObserver]].
 *
 * @tparam T the generic type of value instances for the [[FileTreeRepository]]
 */
private[sbt] class Observers[T] extends Observer[T] with Observable[T] {
  private class Handle(id: Int) extends AutoCloseable {
    override def close(): Unit = {
      observers.synchronized(observers -= id)
      ()
    }
  }
  private[this] val id = new AtomicInteger(0)
  private[this] val observers: mutable.Map[Int, Observer[T]] =
    new util.LinkedHashMap[Int, Observer[T]]().asScala
  private[this] val observables: mutable.Map[AutoCloseable, Unit] =
    new util.WeakHashMap[AutoCloseable, Unit]().asScala

  private[sbt] def addObservable(observable: Observable[T]): AutoCloseable =
    observables.synchronized {
      val handle = observable.addObserver(this)
      observables.put(handle, ())
      handle
    }
  override def addObserver(observer: Observer[T]): AutoCloseable = observers.synchronized {
    val observerId = id.incrementAndGet()
    observers += observerId -> observer
    new Handle(observerId)
  }

  override def close(): Unit = {
    observers.synchronized(observers.clear())
    observables.synchronized {
      observables.keys.foreach(_.close())
      observables.clear()
    }
  }
  override def toString: String =
    s"Observers(\n${observers.map { case (k, v) => s"  $k -> $v" }.mkString("\n")}\n)"
  override def onNext(t: T): Unit = observers.synchronized {
    observers.values.foreach(
      o =>
        try o.onNext(t)
        catch { case NonFatal(_) => })
  }
}
private[sbt] class RegisterableObservable[T](val delegate: Observers[(NioPath, T)]) extends AnyVal {
  def register(glob: Glob): Either[IOException, Observable[(NioPath, T)]] =
    Registerable(glob, delegate)
}

/**
 * A dynamically configured monitor of the file system. New paths can be added and removed from
 * monitoring with register / unregister.
 */
private[sbt] trait Registerable[+T] extends AutoCloseable {

  /**
   * Register a glob for monitoring.
   *
   * @param glob Glob
   * @return an Either that is a Right when register has no errors and a Left if an IOException is
   *         thrown while registering the path. The result should be true if the path has
   *         never been previously registered or if the recursive flag flips from false to true.
   */
  def register(glob: Glob): Either[IOException, Observable[T]]
}
private[sbt] object Registerable {
  def fileEvent[T <: SimpleFileAttributes](
      glob: Glob,
      delegate: Observers[FileEvent[T]]): Either[IOException, Observable[FileEvent[T]]] = {
    val delegateObserver = new Observers[(NioPath, T)]
    val transformation: (NioPath, T) => FileEvent[T] = (path, attributes) =>
      if (attributes.exists) Update(path, attributes, attributes) else Deletion(path, attributes)
    apply(glob, delegateObserver).right.map(Observable.map(_, transformation.tupled))
  }
  def apply[T](glob: Glob,
               delegate: Observers[(NioPath, T)]): Either[IOException, Observable[(NioPath, T)]] = {
    val filter = glob.toFileFilter
    val underlying = new Observers[(NioPath, T)]
    val observers =
      Observable.filter(underlying, (r: (NioPath, T)) => filter.accept(r._1.toFile))
    val handle = delegate.addObserver(underlying)

    Right(new Observable[(NioPath, T)] {
      override def addObserver(observer: Observer[(NioPath, T)]): AutoCloseable =
        observers.addObserver(observer)
      override def close(): Unit = handle.close()
    })
  }
}
