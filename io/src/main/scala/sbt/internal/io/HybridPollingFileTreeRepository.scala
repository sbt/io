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
import java.nio.file.Path

import sbt.io.FileTreeDataView.{ Entry, Observers }
import sbt.io._
import sbt.io.syntax._

import scala.concurrent.duration.FiniteDuration

/**
 * A hybrid [[FileTreeRepository]] that caches some paths and monitors them with os notifications and
 * does not cache the paths that are filtered using the provided shouldPoll function. As a general
 * rule, the paths to be polled should ideally not be in the same directory tree as any of the
 * paths that are being cached. The [[FileTreeRepository.list]] method should do the right thing in this
 * case, but it's possible that there may be some bugs in handling the overlapping paths.
 *
 * @tparam T the type of the [[Entry.value]]s.
 */
private[sbt] trait HybridPollingFileTreeRepository[+T] extends FileTreeRepository[T] { self =>
  def shouldPoll(path: Path): Boolean
  def shouldPoll(typedPath: TypedPath): Boolean = shouldPoll(typedPath.toPath)
  def shouldPoll(glob: Glob): Boolean = shouldPoll(glob.base)
  def toPollingRepository(delay: FiniteDuration, logger: WatchLogger): FileTreeRepository[T]
}

private[io] case class HybridPollingFileTreeRepositoryImpl[+T](converter: TypedPath => T,
                                                               pollingGlobs: Seq[Glob])
    extends HybridPollingFileTreeRepository[T] { self =>
  private val repo = new FileTreeRepositoryImpl[T](converter)
  private val view = DefaultFileTreeView.asDataView(converter)
  private val shouldPollEntry: Entry[_] => Boolean = (e: Entry[_]) => shouldPoll(e.typedPath)

  override def shouldPoll(path: Path): Boolean =
    pollingGlobs.exists(_.toFileFilter(acceptBase = true).accept(path.toFile))
  override def addObserver(observer: FileTreeDataView.Observer[T]): Int =
    repo.addObserver(observer)
  override def register(glob: Glob): Either[IOException, Boolean] = {
    if (shouldPoll(glob.base)) Right(false) else repo.register(glob)
  }
  override def removeObserver(handle: Int): Unit = repo.removeObserver(handle)
  override def listEntries(glob: Glob): Seq[FileTreeDataView.Entry[T]] = {
    if (!shouldPoll(glob.base)) {
      /*
       * The repository may contain some paths that require polling to access. We must remove
       * those entries from the result. For every one of these entries that is a directory, we
       * must poll that directory and add its result to the list. If the entry is a regular file,
       * then we need to poll just that file.
       */
      val (needPoll, ready) = repo.listEntries(glob).partition(shouldPollEntry)
      ready ++ needPoll
        .foldLeft(Set.empty[FileTreeDataView.Entry[T]]) {
          case (entries, e @ Entry(typedPath, _)) if typedPath.isDirectory =>
            val path = typedPath.toPath
            val depth =
              if (glob.depth == Integer.MAX_VALUE) Integer.MAX_VALUE
              else glob.depth - path.relativize(path).getNameCount - 1
            entries ++ (Some(e) ++ view.listEntries(glob.withDepth(depth)))
          case (entries, Entry(typedPath, _))
              if shouldPoll(typedPath) && !shouldPoll(typedPath.toPath.getParent) =>
            entries ++ view.listEntries(typedPath.toPath.toGlob)
          case (entries, _) =>
            entries
        }
        .toSeq
    } else {
      view.listEntries(glob)
    }
  }
  override def list(glob: Glob): Seq[TypedPath] = listEntries(glob).map(_.typedPath)

  override def unregister(glob: Glob): Unit = repo.unregister(glob)
  override def close(): Unit = {
    repo.close()
  }
  override def toPollingRepository(delay: FiniteDuration,
                                   logger: WatchLogger): FileTreeRepository[T] = {
    new FileTreeRepository[T] {
      private val observers = new Observers[T]
      private val handle = self.addObserver(observers)
      private val watchState = WatchState.empty(pollingGlobs, new PollingWatchService(delay))
      private val observable =
        new WatchServiceBackedObservable[T](watchState, delay, converter, true, logger)
      observable.addObserver(observers)

      override def addObserver(observer: FileTreeDataView.Observer[T]): Int =
        observers.addObserver(observer)

      override def removeObserver(handle: Int): Unit = observers.removeObserver(handle)

      override def close(): Unit = {
        observable.close()
        self.removeObserver(handle)
      }

      override def register(glob: Glob): Either[IOException, Boolean] = {
        if (self.shouldPoll(glob)) observable.register(glob)
        self.register(glob)
      }
      override def unregister(glob: Glob): Unit = {
        observable.unregister(glob)
        self.unregister(glob)
      }
      override def listEntries(glob: Glob): Seq[FileTreeDataView.Entry[T]] = self.listEntries(glob)
      override def list(glob: Glob): Seq[TypedPath] = self.list(glob)
    }
  }
}

private[sbt] object HybridPollingFileTreeRepository {
  def apply[T](converter: TypedPath => T, pollingGlobs: Glob*): HybridPollingFileTreeRepository[T] =
    HybridPollingFileTreeRepositoryImpl(converter, pollingGlobs)
}
