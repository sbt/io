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

import java.io.IOException
import java.nio.file.{ Path => NioPath }
import java.util.WeakHashMap
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import sbt.nio.file.Glob
import sbt.nio.file.Glob.GlobOps

import scala.util.control.NonFatal

@FunctionalInterface
private[sbt] trait Observer[-T] extends AutoCloseable {

  /**
   * Run a callback
   * @param t the generic type of events
   */
  def onNext(t: T): Unit

  /**
   * This is for managed observers that may need to be removed once the caller is done with them.
   * See [[Observers]].
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
      observable.addObserver(t => observer.onNext(f(t)))
    override def close(): Unit = observable.close()
  }
  def filter[T](observable: Observable[T], f: T => Boolean): Observable[T] = new Observable[T] {
    override def addObserver(observer: Observer[T]): AutoCloseable =
      observable.addObserver(t => if (f(t)) observer.onNext(t))
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
      observers.remove(id)
      ()
    }
  }
  private[this] val id = new AtomicInteger(0)
  private[this] val observers = new ConcurrentHashMap[Int, Observer[T]]
  private[this] val observables = new WeakHashMap[AutoCloseable, Unit]

  private[sbt] def addObservable(observable: Observable[T]): AutoCloseable =
    observables.synchronized {
      val handle = observable.addObserver(this)
      observables.put(handle, ())
      handle
    }
  override def addObserver(observer: Observer[T]): AutoCloseable = {
    val observerId = id.incrementAndGet()
    observers.put(observerId, observer)
    new Handle(observerId)
  }

  override def close(): Unit = {
    observers.clear()
    observables.synchronized {
      observables.keySet.forEach(_.close())
      observables.clear()
    }
  }
  override def toString: String =
    s"Observers(observers = ${observers.values}, observables = ${observables.keySet})"
  override def onNext(t: T): Unit = observers.values.forEach { o =>
    try o.onNext(t)
    catch { case NonFatal(_) => }
  }
}
private[sbt] class RegisterableObservable[T](val delegate: Observers[FileEvent[T]]) extends AnyVal {
  def register(glob: Glob): Either[IOException, Observable[FileEvent[T]]] =
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
  def apply[T](
      glob: Glob,
      delegate: Observers[FileEvent[T]]
  ): Either[IOException, Observable[FileEvent[T]]] = {
    val filter = glob.toFileFilter
    val underlying = new Observers[FileEvent[T]]
    val observers =
      Observable.filter(underlying, (e: FileEvent[T]) => filter.accept(e.path.toFile))
    val handle = delegate.addObserver(underlying)

    Right(new Observable[FileEvent[T]] {
      override def addObserver(observer: Observer[FileEvent[T]]): AutoCloseable =
        observers.addObserver(observer)
      override def close(): Unit = handle.close()
    })
  }
}
