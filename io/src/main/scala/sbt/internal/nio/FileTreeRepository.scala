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
import java.nio.file.Path

import sbt.internal.nio.FileEvent.{ Creation, Deletion, Update }
import sbt.io.WatchService
import sbt.nio.{ FileAttributes, Glob }

import scala.util.Try

/**
 * Monitors registered directories for file changes. A typical implementation will keep an
 * in memory cache of the file system that can be queried in [[FileTreeRepository!.list]]. The
 * [[FileTreeRepository#register]] method adds monitoring for a particular cache. A filter may be
 * provided so that the cache doesn't waste memory on files the user doesn't care about. The
 * cache may be shared across a code base so there additional apis for adding filters or changing
 * the recursive property of a directory.
 *
 * @tparam T the type of the
 */
private[sbt] trait FileTreeRepository[+T]
    extends NioFileTreeView[T]
    with Registerable[FileEvent[T]]
    with Observable[FileEvent[T]]
    with AutoCloseable

private[sbt] object FileTreeRepository {
  private[sbt] implicit class Ops[T](val repo: FileTreeRepository[T]) extends AnyVal {
    def map[U](f: T => U, closeUnderlying: Boolean): FileTreeRepository[U] =
      new FileTreeRepository[U] {
        private val observers = new Observers[FileEvent[U]]
        private val handle = repo.addObserver((_: FileEvent[T]) match {
          case Creation(path, attributes) => observers.onNext(Creation(path, f(attributes)))
          case Deletion(path, attributes) => observers.onNext(Deletion(path, f(attributes)))
          case Update(path, previousAttributes, attributes) =>
            observers.onNext(Update(path, f(previousAttributes), f(attributes)))
        })
        override def register(glob: Glob): Either[IOException, Observable[FileEvent[U]]] =
          repo
            .register(glob)
            .map((o: Observable[FileEvent[T]]) => Observable.map(o, (_: FileEvent[T]).map(f)))
        override def addObserver(observer: Observer[FileEvent[U]]): AutoCloseable =
          observers.addObserver(observer)
        override def list(glob: Glob, filter: ((Path, U)) => Boolean): Seq[(Path, U)] =
          repo.list(glob, _ => true).flatMap {
            case (p, t) =>
              Some(p -> f(t)).filter(filter)
          }
        override def close(): Unit = {
          handle.close()
          if (closeUnderlying) repo.close()
        }
      }
  }

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for the
   * monitored directories.
   *
   * @param converter function to generate an instance of `T` from a
   *                  `(Path, BasicFileAttributes)` pair
   * @tparam T the generic type of the data value associated with each file
   * @return a file repository.
   */
  private[sbt] def default[T: Manifest](
      converter: (Path, FileAttributes) => Try[T]): FileTreeRepository[(FileAttributes, Try[T])] =
    new FileTreeRepositoryImpl[T](converter)

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for the
   * monitored directories.
   *
   * @param converter function to generate a cache data value from a
   *                  `(Path, FileAttributes)` pair
   * @tparam T the generic type of the
   * @return a file repository.
   */
  private[sbt] def legacy[T](
      converter: (Path, FileAttributes) => Try[T]): FileTreeRepository[(FileAttributes, Try[T])] =
    new LegacyFileTreeRepository[T](converter, (_: Any) => (), WatchService.default)

  /**
   * Create a [[FileTreeRepository]] with a provided logger. The generated repository will cache
   * the file system tree for the monitored directories.
   *
   * @param converter function to generate a cache data value from a
   *                  `(Path, FileAttributes)` pair
   * @param logger used to log file events
   * @param watchService the [[WatchService]] to monitor for file system events
   * @tparam T the generic type of the custom file attributes
   * @return a file repository.
   */
  private[sbt] def legacy[T](
      converter: (Path, FileAttributes) => Try[T],
      logger: WatchLogger,
      watchService: WatchService): FileTreeRepository[(FileAttributes, Try[T])] =
    new LegacyFileTreeRepository[T](converter, logger, watchService)

}
