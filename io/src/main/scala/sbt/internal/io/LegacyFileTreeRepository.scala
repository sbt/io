package sbt.internal.io

import java.io.IOException
import java.nio.file.{ Path, WatchKey }
import java.util.concurrent.ConcurrentHashMap

import sbt.io._

import scala.collection.JavaConverters._
import scala.concurrent.duration._

/**
 * A [[FileTreeRepository]] that can be used to emulate the behavior of sbt < 1.3.0 The list methods
 * will always poll the file system and the monitoring will be handled by a
 * [[WatchServiceBackedObservable]].
 */
private[sbt] class LegacyFileTreeRepository[+T](converter: TypedPath => T,
                                                logger: WatchLogger,
                                                watchService: WatchService)
    extends FileTreeRepository[T] {
  private[this] val view = FileTreeView.DEFAULT.asDataView(converter)
  private[this] val globs = ConcurrentHashMap.newKeySet[Glob].asScala
  private[this] val observable =
    new WatchServiceBackedObservable[T](
      new NewWatchState(globs, watchService, new ConcurrentHashMap[Path, WatchKey].asScala),
      100.millis,
      converter,
      closeService = true,
      logger)

  override def addObserver(observer: FileTreeDataView.Observer[T]): Int =
    observable.addObserver(observer)
  override def close(): Unit = observable.close()
  override def list(glob: Glob): Seq[TypedPath] = view.list(glob)
  override def listEntries(glob: Glob): Seq[FileTreeDataView.Entry[T]] = view.listEntries(glob)
  override def register(glob: Glob): Either[IOException, Boolean] = {
    globs.add(glob)
    observable.register(glob)
  }
  override def removeObserver(handle: Int): Unit = observable.removeObserver(handle)
  override def unregister(glob: Glob): Unit = {
    globs.remove(glob)
    observable.unregister(glob)
  }
}
