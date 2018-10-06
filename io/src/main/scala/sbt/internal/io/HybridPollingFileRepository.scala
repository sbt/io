package sbt.internal.io

import java.io.IOException
import java.nio.file.Path

import sbt.io.FileTreeDataView.{ Entry, Observable }
import sbt.io._

import scala.concurrent.duration.FiniteDuration

/**
 * A hybrid [[FileRepository]] that caches some paths and monitors them with os notifications and
 * does not cache the paths that are filtered using the provided shouldPoll function. As a general
 * rule, the paths to be polled should ideally not be in the same directory tree as any of the
 * paths that are being cached. The [[FileRepository.list]] method should do the right thing in this
 * case, but it's possible that there may be some bugs in handling the overlapping paths.
 *
 * @tparam T the type of the [[Entry.value]]s.
 */
private[sbt] trait HybridPollingFileRepository[+T] extends FileRepository[T] { self =>
  def shouldPoll(path: Path): Boolean
  def shouldPoll(typedPath: TypedPath): Boolean = shouldPoll(typedPath.getPath)
  def shouldPoll(source: Source): Boolean = shouldPoll(source.base.toPath)
  def toPollingObservable(delay: FiniteDuration,
                          sources: Seq[Source],
                          logger: WatchLogger): Observable[T]
}

private[io] case class HybridPollingFileRepositoryImpl[+T](converter: TypedPath => T,
                                                           pollingSources: Seq[Source])
    extends HybridPollingFileRepository[T] { self =>
  private val repo = new FileRepositoryImpl[T](converter)
  private val view = DefaultFileTreeView.asDataView(converter)
  private val shouldPollEntry: Entry[_] => Boolean = (e: Entry[_]) => shouldPoll(e.typedPath)

  override def shouldPoll(path: Path): Boolean = pollingSources.exists(_.accept(path))
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
      ready ++ needPoll.flatMap {
        case e @ Entry(typedPath, _) if typedPath.isDirectory =>
          val path = typedPath.getPath
          val depth =
            if (maxDepth == Integer.MAX_VALUE) Integer.MAX_VALUE
            else maxDepth - path.relativize(path).getNameCount - 1
          Some(e).filter(filter) ++
            view.listEntries(path, depth, (e: Entry[T]) => shouldPollEntry(e) && filter(e))
        case Entry(typedPath, _)
            if shouldPoll(typedPath) && !shouldPoll(typedPath.getPath.getParent) =>
          view.listEntries(typedPath.getPath, -1, (_: Entry[T]) => true)
        case _ =>
          Nil
      }
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
                          sources: Seq[Source],
                          logger: WatchLogger): Observable[T] = {
    val pollingSources = sources.filter(shouldPoll)
    if (pollingSources.isEmpty) self
    else {
      new Observable[T] {
        private val observers = new Observers[T]
        private val handle = self.addObserver(observers)
        private val watchState = WatchState.empty(new PollingWatchService(delay), pollingSources)
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

private[sbt] object HybridPollingFileRepository {
  def apply[T](converter: TypedPath => T, pollingSources: Source*): HybridPollingFileRepository[T] =
    HybridPollingFileRepositoryImpl(converter, pollingSources)
}
