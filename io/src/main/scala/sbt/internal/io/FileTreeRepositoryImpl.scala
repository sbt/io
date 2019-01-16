package sbt.internal.io

import java.io.IOException
import java.nio.file.{ Path => JPath }
import java.util.concurrent.atomic.AtomicBoolean

import com.swoval.files.FileTreeDataViews.Converter
import com.swoval.files.{ FileTreeRepositories, TypedPath => STypedPath }
import com.swoval.functional.Filters
import sbt.internal.io.SwovalConverters.{ ObserverOps, SwovalEitherOps, SwovalEntryOps }
import sbt.io.FileTreeDataView.Entry
import sbt.io.{ FileTreeDataView, FileTreeRepository, TypedPath }

import scala.collection.immutable.VectorBuilder

/**
 * The default implemenation of [[FileTreeRepository]]. It delegates all of its methods to the
 * [[https://swoval.github.io/files/jvm/com/swoval/files/FileTreeRepository.html swoval FileTreeRepository]].
 *
 * @param converter the function to convert paths to
 * @tparam T the type of the [[FileTreeDataView.Entry.value]]s.
 */
private[sbt] class FileTreeRepositoryImpl[+T](converter: TypedPath => T)
    extends FileTreeRepository[T] {
  private[this] val closed = new AtomicBoolean(false)
  private[this] val underlying = FileTreeRepositories.get[T](new Converter[T] {
    import SwovalConverters.SwovalTypedPathOps
    override def apply(path: STypedPath): T = converter(path.asSbt)
  }, true)

  override def addObserver(observer: FileTreeDataView.Observer[T]): Int = {
    throwIfClosed("addObserver")
    underlying.addCacheObserver(observer.asSwoval)
  }
  override def list(path: JPath, maxDepth: Int, filter: TypedPath => Boolean): Seq[TypedPath] = {
    throwIfClosed("list")
    listEntries(path, maxDepth, (e: Entry[T]) => filter(e.typedPath)).map(_.typedPath)
  }
  override def listEntries(path: JPath,
                           maxDepth: Int,
                           filter: Entry[T] => Boolean): Seq[Entry[T]] = {
    throwIfClosed("listEntries")
    val res = new VectorBuilder[Entry[T]]
    val it = underlying.listEntries(path, maxDepth, Filters.AllPass).iterator
    while (it.hasNext) {
      val entry: Entry[T] = it.next.asSbt
      if (filter(entry)) res += entry
    }
    res.result
  }
  override def register(path: JPath, maxDepth: Int): Either[IOException, Boolean] = {
    throwIfClosed("register")
    underlying.register(path, maxDepth).asScala
  }
  override def removeObserver(handle: Int): Unit = {
    throwIfClosed("removeObserver")
    underlying.removeObserver(handle)
  }
  override def unregister(path: JPath): Unit = {
    throwIfClosed("unregister")
    underlying.unregister(path)
  }
  override def close(): Unit = if (closed.compareAndSet(false, true)) {
    underlying.close()
  }
  private[this] def throwIfClosed(method: String): Unit =
    if (closed.get()) {
      val ex = new IllegalStateException(s"Tried to invoke $method on closed repostitory $this")
      ex.printStackTrace()
      throw ex
    }
}
