package sbt.internal.io

import java.io.IOException
import java.util.concurrent.atomic.AtomicBoolean

import com.swoval.files.FileTreeDataViews.Converter
import com.swoval.files.{ FileTreeRepositories, TypedPath => STypedPath }
import com.swoval.functional.Filters
import sbt.internal.io.SwovalConverters.{ ObserverOps, SwovalEitherOps, SwovalEntryOps }
import sbt.io.FileTreeDataView.Entry
import sbt.io.{ FileTreeDataView, FileTreeRepository, Glob, TypedPath }

import scala.collection.immutable.VectorBuilder
import scala.collection.JavaConverters._

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
  override def list(glob: Glob): Seq[TypedPath] = {
    throwIfClosed("list")
    listEntries(glob).map(_.typedPath)
  }
  override def listEntries(glob: Glob): Seq[Entry[T]] = {
    throwIfClosed("listEntries")
    val res = new VectorBuilder[Entry[T]]
    underlying
      .listEntries(glob.base, glob.depth, Filters.AllPass)
      .iterator
      .asScala
      .foreach(e => res += e.asSbt)
    res.result
  }
  override def register(glob: Glob): Either[IOException, Boolean] = {
    throwIfClosed("register")
    underlying.register(glob.base, glob.depth).asScala
  }
  override def removeObserver(handle: Int): Unit = {
    throwIfClosed("removeObserver")
    underlying.removeObserver(handle)
  }
  override def unregister(glob: Glob): Unit = {
    throwIfClosed("unregister")
    underlying.unregister(glob.base)
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
