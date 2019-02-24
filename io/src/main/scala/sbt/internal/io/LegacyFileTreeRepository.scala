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
import java.nio.file.{ Path, Paths, WatchKey }
import java.util
import java.util.concurrent.{ ConcurrentHashMap, ConcurrentSkipListMap }

import sbt.internal.io.FileEvent.{ Creation, Deletion, Update }
import sbt.internal.io.FileTreeView.AllPass
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
  private[this] val files =
    util.Collections.synchronizedSortedMap(new ConcurrentSkipListMap[Path, CustomFileAttributes[T]])
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
        val events: Seq[(Path, FileEvent[CustomFileAttributes[T]])] =
          files.synchronized(files.get(path)) match {
            case null =>
              val res = (path -> Creation(path, attrs)) +: getCreations(path, attrs)
              files.synchronized {
                val subMap = files.subMap(path, ceiling(path))
                res.foreach { case (_, FileEvent(p, a, _)) => subMap.put(p, a) }
              }
              res
            case prevAttrs if attrs.exists => path -> Update(path, prevAttrs, attrs) :: Nil
            case prevAttrs =>
              files.synchronized {
                val res = (path -> Deletion(path, prevAttrs)) +: getDeletions(path, prevAttrs)
                res.foreach { case (p, _) => files.remove(p) }
                res
              }
          }
        events.foreach(observers.onNext)
      }
    })
  private[this] def globFilter: Path => Boolean = path => globs.exists(_.filter(path))
  // This is a mildly hacky way of specifying an upper bound for children of a path
  private[this] def ceiling(path: Path): Path = Paths.get(path.toString + Char.MaxValue)
  private def getDeletions(
      path: Path,
      attributes: SimpleFileAttributes): Seq[(Path, FileEvent[CustomFileAttributes[T]])] = {
    if (attributes.isDirectory()) {
      val floor = path.resolve("")
      val children = files.subMap(floor, ceiling(path)).asScala.toSeq
      children.map { case (p, v) => p -> Deletion(p, v) }
    } else Nil
  }
  private def getCreations(
      path: Path,
      attributes: SimpleFileAttributes): Seq[(Path, FileEvent[CustomFileAttributes[T]])] = {
    if (attributes.isDirectory())
      view.list(Glob(path, Int.MaxValue, globFilter), AllPass).map {
        case (p, a) => p -> Creation(p, a)
      } else Nil
  }

  override def close(): Unit = {
    handle.close()
    observable.close()
  }
  override def register(
      glob: Glob): Either[IOException, Observable[(Path, FileEvent[CustomFileAttributes[T]])]] = {
    val allPassGlob = glob.withFilter(AllPassFilter)
    if (globs.add(allPassGlob)) {
      val map = new util.HashMap[Path, CustomFileAttributes[T]].asScala
      map ++= view.list(allPassGlob, AllPass)
      files.putAll(map.asJava)
    }
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
