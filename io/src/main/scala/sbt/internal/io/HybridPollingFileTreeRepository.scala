package sbt.internal.io

import java.io.IOException
import java.nio.file.Path

import sbt.io.FileTreeDataView.{ Entry, Observable, Observers }
import sbt.io._

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
  def toPollingObservable(delay: FiniteDuration,
                          globs: Seq[Glob],
                          logger: WatchLogger): Observable[T]
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
  override def register(path: Path, maxDepth: Int): Either[IOException, Boolean] = {
    if (shouldPoll(path)) Right(false) else repo.register(path, maxDepth)
  }
  override def removeObserver(handle: Int): Unit = repo.removeObserver(handle)
  override def listEntries(path: Path,
                           maxDepth: Int,
                           filter: Entry[T] => Boolean): Seq[FileTreeDataView.Entry[T]] = {
    if (!shouldPoll(path)) {
      /*
       * The repository may contain some paths that require polling to access. We must remove
       * those entries from the result. For every one of these entries that is a directory, we
       * must poll that directory and add its result to the list. If the entry is a regular file,
       * then we need to poll just that file.
       */
      val (needPoll, ready) =
        repo
          .listEntries(path, maxDepth, (e: Entry[T]) => filter(e) || shouldPollEntry(e))
          .partition(shouldPollEntry)
      ready ++ needPoll
        .foldLeft(Set.empty[FileTreeDataView.Entry[T]]) {
          case (entries, e @ Entry(typedPath, _)) if typedPath.isDirectory =>
            val path = typedPath.toPath
            val depth = 0
            if (maxDepth == Integer.MAX_VALUE) Integer.MAX_VALUE
            else maxDepth - path.relativize(path).getNameCount - 1
            entries ++ (Some(e).filter(filter) ++
              view.listEntries(path, depth, (e: Entry[T]) => shouldPollEntry(e) && filter(e)))
          case (entries, Entry(typedPath, _))
              if shouldPoll(typedPath) && !shouldPoll(typedPath.toPath.getParent) =>
            entries ++ view.listEntries(typedPath.toPath, -1, (_: Entry[T]) => true)
          case (entries, _) =>
            entries
        }
        .toSeq
    } else {
      view.listEntries(path, maxDepth, (e: Entry[T]) => shouldPollEntry(e) && filter(e))
    }
  }
  override def list(path: Path, maxDepth: Int, filter: TypedPath => Boolean): Seq[TypedPath] =
    listEntries(path, maxDepth, (e: Entry[T]) => filter(e.typedPath)).map(_.typedPath)

  override def unregister(path: Path): Unit = repo.unregister(path)
  override def close(): Unit = {
    repo.close()
  }
  def toPollingObservable(delay: FiniteDuration,
                          globs: Seq[Glob],
                          logger: WatchLogger): Observable[T] = {
    val pollingGlobs = globs.filter(shouldPoll)
    if (pollingGlobs.isEmpty) self
    else {
      new Observable[T] {
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
      }
    }
  }
}

private[sbt] object HybridPollingFileTreeRepository {
  def apply[T](converter: TypedPath => T, pollingGlobs: Glob*): HybridPollingFileTreeRepository[T] =
    HybridPollingFileTreeRepositoryImpl(converter, pollingGlobs)
}
