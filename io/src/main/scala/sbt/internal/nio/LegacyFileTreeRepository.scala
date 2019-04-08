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
import java.nio.file.{ Path, WatchKey }
import java.util.concurrent.ConcurrentHashMap

import sbt.internal.io._
import sbt.internal.nio.FileEvent.Deletion
import sbt.io._
import sbt.nio.FileAttributes.NonExistent
import sbt.nio.{ FileAttributes, FileTreeView, Glob }

import scala.collection.JavaConverters._
import scala.concurrent.duration._

/**
 * A [[FileTreeRepository]] that can be used to emulate the behavior of sbt < 1.3.0 The list methods
 * will always poll the file system and the monitoring will be handled by a
 * [[WatchServiceBackedObservable]].
 */
private[sbt] class LegacyFileTreeRepository(logger: WatchLogger, watchService: WatchService)
    extends FileTreeRepository[FileAttributes] {
  private[this] val view: FileTreeView.Nio[FileAttributes] = FileTreeView.DEFAULT_NIO
  private[this] val globs = ConcurrentHashMap.newKeySet[Glob].asScala
  private[this] val fileCache = new FileCache(p => FileAttributes(p).getOrElse(NonExistent), globs)
  private[this] val observable
    : Observable[FileEvent[FileAttributes]] with Registerable[FileEvent[FileAttributes]] =
    new WatchServiceBackedObservable(
      new NewWatchState(globs, watchService, new ConcurrentHashMap[Path, WatchKey].asScala),
      100.millis,
      closeService = true,
      logger
    )
  private[this] val observers = new Observers[FileEvent[FileAttributes]]
  private[this] val handle =
    observable.addObserver((event: FileEvent[FileAttributes]) => {
      val attributes = event match {
        case _: Deletion[_] => NonExistent
        case _              => event.attributes
      }
      val events: Seq[FileEvent[FileAttributes]] = fileCache.update(event.path, attributes)
      events.foreach(observers.onNext)
    })
  override def close(): Unit = {
    handle.close()
    observable.close()
  }
  override def register(glob: Glob): Either[IOException, Observable[FileEvent[FileAttributes]]] = {
    fileCache.register(glob)
    observable.register(glob).right.foreach(_.close())
    new RegisterableObservable(observers).register(glob)
  }
  override def list(path: Path): Seq[(Path, FileAttributes)] = view.list(path)

  /**
   * Add callbacks to be invoked on file events.
   *
   * @param observer the callbacks to invoke
   * @return a handle to the callback.
   */
  override def addObserver(observer: Observer[FileEvent[FileAttributes]]): AutoCloseable =
    observers.addObserver(observer)
}
