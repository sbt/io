/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.io

import java.nio.file.{ Path => NioPath }

import sbt.internal.io._

/**
 * Provides a view into the file system that allows retrieval of the children of a particular path.
 * Specific implementations may or may not use a cache for retrieval.
 */
private[sbt] trait FileTreeView[+T] extends AutoCloseable {

  /**
   * List the contents of the current directory.
   *
   * @param glob The glob to list
   * @return a sequence of `(Path, T) tuples`
   */
  def list(glob: Glob, filter: T => Boolean): Seq[T]

  // Many, if not most, FileTreeViews should not create new resources.
  override def close(): Unit = {}
}

private[sbt] trait NioFileTreeView[+T] extends FileTreeView[(NioPath, T)] {
  def list(glob: Glob, filter: ((NioPath, T)) => Boolean): Seq[(NioPath, T)]
  final def list(glob: Glob, filter: (NioPath, T) => Boolean): Seq[(NioPath, T)] =
    list(glob, filter.tupled)
}

private[sbt] object FileTreeView {
  object AllPass extends (Any => Boolean) {
    override def apply(any: Any): Boolean = true
    override def toString: String = "AllPass"
  }
  private[sbt] val DEFAULT: NioFileTreeView[SimpleFileAttributes] = DefaultFileTreeView
  private class MappedFileTreeView[+T, +R](view: FileTreeView[T],
                                           converter: T => R,
                                           closeUnderlying: Boolean)
      extends FileTreeView[R] {
    override def list(glob: Glob, filter: R => Boolean): Seq[R] = {
      view.list(glob, AllPass).flatMap { t =>
        val r: R = converter(t)
        if (filter(r)) r :: Nil else Nil
      }
    }
    override def close(): Unit = if (closeUnderlying) view.close()
  }
  private[sbt] implicit class NioFileTreeViewOps[T](val view: NioFileTreeView[T]) {
    def map[A >: T, B](f: (NioPath, A) => B): NioFileTreeView[B] = {
      val mapped: FileTreeView[(NioPath, B)] = {
        val converter: ((NioPath, A)) => (NioPath, B) = {
          case (path: NioPath, attrs) => path -> f(path, attrs)
        }
        new MappedFileTreeView(view, converter, true)
      }
      new NioFileTreeView[B] {
        override def list(glob: Glob, filter: ((NioPath, B)) => Boolean): Seq[(NioPath, B)] =
          mapped.list(glob, filter)
      }
    }
    def flatMap[B, A >: T](f: (NioPath, A) => Traversable[B]): NioFileTreeView[B] = {
      val converter: ((NioPath, A)) => Traversable[(NioPath, B)] = {
        case (path: NioPath, attrs) => f(path, attrs).map(path -> _)
      }
      new NioFileTreeView[B] {
        override def list(glob: Glob, filter: ((NioPath, B)) => Boolean): Seq[(NioPath, B)] =
          view.list(glob, AllPass).flatMap(converter(_).filter(filter))
      }
    }
  }
}

// scaladoc is horrible and I couldn't figure out how to link the overloaded method listEntries
// in this message.
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
    with Registerable[(NioPath, FileEvent[T])]
    with Observable[(NioPath, FileEvent[T])]
    with AutoCloseable

private[sbt] object FileTreeRepository {

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
      converter: (NioPath, SimpleFileAttributes) => CustomFileAttributes[T])
    : FileTreeRepository[CustomFileAttributes[T]] =
    new FileTreeRepositoryImpl[T](converter)

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for the
   * monitored directories.
   *
   * @param converter function to generate [[CustomFileAttributes]] from a
   *                  `(Path, SimpleFileAttributes)` pair
   * @tparam T the generic type of the
   * @return a file repository.
   */
  private[sbt] def legacy[T](converter: (NioPath, SimpleFileAttributes) => CustomFileAttributes[T])
    : FileTreeRepository[CustomFileAttributes[T]] =
    new LegacyFileTreeRepository[T](converter, new WatchLogger {
      override def debug(msg: => Any): Unit = {}
    }, WatchService.default)

  /**
   * Create a [[FileTreeRepository]] with a provided logger. The generated repository will cache
   * the file system tree for the monitored directories.
   *
   * @param converter function to generate [[CustomFileAttributes]] from a
   *                  `(Path, SimpleFileAttributes)` pair
   * @param logger used to log file events
   * @param watchService the [[WatchService]] to monitor for file system events
   * @tparam T the generic type of the custom file attributes
   * @return a file repository.
   */
  private[sbt] def legacy[T](
      converter: (NioPath, SimpleFileAttributes) => CustomFileAttributes[T],
      logger: WatchLogger,
      watchService: WatchService): FileTreeRepository[CustomFileAttributes[T]] =
    new LegacyFileTreeRepository[T](converter, logger, watchService)

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for some
   * of the paths under monitoring, but others will need to be polled.
   *
   * @param converter function to generate [[CustomFileAttributes]] from a
   *                  `(Path, SimpleFileAttributes)` pair
   * @param pollingGlobs do not cache any path contained in these [[Glob]]s.
   * @tparam T the generic type of the custom file attributes
   * @return a file repository.
   */
  private[sbt] def hybrid[T](converter: (NioPath, SimpleFileAttributes) => CustomFileAttributes[T],
                             pollingGlobs: Glob*): HybridPollingFileTreeRepository[T] =
    HybridPollingFileTreeRepository(converter, pollingGlobs: _*)
}
