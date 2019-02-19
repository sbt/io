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
import java.nio.file.{ Path, WatchKey }
import java.util.concurrent.ConcurrentHashMap

import sbt.internal.io.FileEvent.{ Deletion, Update }
import sbt.io._

import scala.collection.JavaConverters._
import scala.concurrent.duration._

/**
 * A [[FileTreeRepository]] that can be used to emulate the behavior of sbt < 1.3.0 The list methods
 * will always poll the file system and the monitoring will be handled by a
 * [[WatchServiceBackedObservable]].
 */
private[sbt] class LegacyFileTreeRepository[+T](
    converter: (Path, SimpleFileAttributes) => CustomFileAttributes[T],
    logger: WatchLogger,
    watchService: WatchService)
    extends FileTreeRepository[CustomFileAttributes[T]] {
  private[this] val view: NioFileTreeView[CustomFileAttributes[T]] =
    FileTreeView.DEFAULT.map((p: Path, a: SimpleFileAttributes) => converter(p, a))
  private[this] val globs = ConcurrentHashMap.newKeySet[Glob].asScala
  private[this] val observable: Observable[(Path, CustomFileAttributes[T])]
    with Registerable[(Path, CustomFileAttributes[T])] =
    new WatchServiceBackedObservable[CustomFileAttributes[T]](
      new NewWatchState(globs, watchService, new ConcurrentHashMap[Path, WatchKey].asScala),
      100.millis,
      (path: Path, attributes: SimpleFileAttributes) => converter(path, attributes),
      closeService = true,
      logger
    )
  private[this] val observers = new Observers[(Path, FileEvent[CustomFileAttributes[T]])]
  private[this] val handle =
    observable.addObserver(new Observer[(Path, CustomFileAttributes[T])] {
      override def onNext(tuple: (Path, CustomFileAttributes[T])): Unit = {
        val (path, attrs) = tuple
        attrs.value match {
          case Right(_) =>
            val event = if (attrs.exists) Update(path, attrs, attrs) else Deletion(path, attrs)
            observers.onNext(path -> event)
          case _ =>
            observers.onNext(path -> Deletion(path, attrs))
        }
      }
    })

  override def close(): Unit = {
    handle.close()
    observable.close()
  }
  override def register(
      glob: Glob): Either[IOException, Observable[(Path, FileEvent[CustomFileAttributes[T]])]] = {
    globs.add(glob)
    observable.register(glob).right.foreach(_.close())
    new RegisterableObservable(observers).register(glob)
  }
  override def list(
      glob: Glob,
      filter: ((Path, CustomFileAttributes[T])) => Boolean): Seq[(Path, CustomFileAttributes[T])] =
    view.list(glob, filter)

  /**
   * Add callbacks to be invoked on file events.
   *
   * @param observer the callbacks to invoke
   * @return a handle to the callback.
   */
  override def addObserver(
      observer: Observer[(Path, FileEvent[CustomFileAttributes[T]])]): AutoCloseable =
    observers.addObserver(observer)
}
