package sbt.internal.io

import java.io.IOException
import java.nio.file.{ Path => JPath }

import com.swoval.files.FileTreeDataViews.{ CacheObserver, Entry => SEntry }
import com.swoval.files.{ TypedPath => STypedPath }
import com.swoval.functional.{ Either => SEither }
import sbt.io.FileTreeDataView.{ Entry, Observer }
import sbt.io.TypedPath
import sbt.io.TypedPath.MetaTypedPath

/**
 * Utilities for converting between swoval and sbt data types.
 */
private[io] object SwovalConverters {
  implicit class SwovalTypedPathOps(val typedPath: STypedPath) extends AnyVal {
    def asSbt: TypedPath = new TypedPath with MetaTypedPath {
      override def toPath: JPath = typedPath.getPath
      override def exists: Boolean = typedPath.exists()
      override def isDirectory: Boolean = typedPath.isDirectory
      override def isFile: Boolean = typedPath.isFile
      override def isSymbolicLink: Boolean = typedPath.isSymbolicLink
      override def toString: String = s"TypedPath($toPath)"
    }
  }

  implicit class SwovalEntryOps[T](val entry: SEntry[T]) extends AnyVal {
    def asSbt: Entry[T] = Entry(entry.getTypedPath.asSbt, entry.getValue.asScala)
  }

  implicit class SwovalEitherOps[L, R](val either: SEither[L, R]) extends AnyVal {
    def asScala[R0](implicit f: R => R0): Either[L, R0] = either match {
      case l: com.swoval.functional.Either.Left[L, R] =>
        Left(com.swoval.functional.Either.leftProjection(l).getValue)
      case r: com.swoval.functional.Either.Right[L, R] => Right(f(r.get()))
    }
  }

  private class DelegateCacheObserver[T](observer: Observer[T]) extends CacheObserver[T] {
    override def onCreate(newEntry: SEntry[T]): Unit = observer.onCreate(newEntry.asSbt)
    override def onDelete(oldEntry: SEntry[T]): Unit = observer.onDelete(oldEntry.asSbt)
    override def onUpdate(oldEntry: SEntry[T], newEntry: SEntry[T]): Unit =
      observer.onUpdate(oldEntry.asSbt, newEntry.asSbt)
    override def onError(exception: IOException): Unit = {}
  }
  implicit class ObserverOps[T](val observer: Observer[T]) extends AnyVal {
    def asSwoval: CacheObserver[T] = new DelegateCacheObserver[T](observer)
  }
}
