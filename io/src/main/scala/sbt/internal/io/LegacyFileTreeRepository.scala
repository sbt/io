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
  override def listEntries(
      path: Path,
      maxDepth: Int,
      filter: FileTreeDataView.Entry[T] => Boolean): Seq[FileTreeDataView.Entry[T]] =
    view.listEntries(path, maxDepth, filter)

  override def list(path: Path, maxDepth: Int, filter: TypedPath => Boolean): Seq[TypedPath] =
    view.list(path, maxDepth, filter)

  override def addObserver(observer: FileTreeDataView.Observer[T]): Int =
    observable.addObserver(observer)

  override def removeObserver(handle: Int): Unit = observable.removeObserver(handle)
  override def close(): Unit = observable.close()

  override def register(path: Path, maxDepth: Int): Either[IOException, Boolean] = {
    globs.add(Glob(path.toFile, AllPassFilter, maxDepth))
    observable.register(path, maxDepth)
  }
  override def unregister(path: Path): Unit = {
    val allGlobs = globs.toIndexedSeq
    allGlobs.foreach(g => if (g.base == path) globs -= g)
    observable.unregister(path)
  }
}
