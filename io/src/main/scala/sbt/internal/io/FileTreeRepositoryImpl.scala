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
import java.nio.file.{ Path => NioPath }
import java.util.concurrent.atomic.AtomicBoolean

import com.swoval.files.FileTreeDataViews.{ CacheObserver, Converter }
import com.swoval.files.{ FileTreeDataViews, FileTreeRepositories, TypedPath => STypedPath }
import com.swoval.functional.Filters
import sbt.internal.io.FileEvent.{ Creation, Deletion, Update }
import sbt.internal.io.SwovalConverters._
import sbt.io._

import scala.collection.JavaConverters._
import scala.collection.immutable.VectorBuilder

/**
 * The default implemenation of [[FileTreeRepository]]. It delegates all of its methods to the
 * [[https://swoval.github.io/files/jvm/com/swoval/files/FileTreeRepository.html swoval FileTreeRepository]].
 *
 * @param converter the function to convert paths to
 * @tparam T the type of the values.
 */
private[sbt] class FileTreeRepositoryImpl[+T](
    converter: (NioPath, SimpleFileAttributes) => CustomFileAttributes[T])
    extends FileTreeRepository[CustomFileAttributes[T]] {
  private[this] val closed = new AtomicBoolean(false)
  private[this] val underlying = FileTreeRepositories.get[CustomFileAttributes[T]](
    new Converter[CustomFileAttributes[T]] {
      override def apply(typedPath: STypedPath): CustomFileAttributes[T] = {
        val path = typedPath.getPath
        val simpleFileAttributes = SimpleFileAttributes.get(typedPath.exists,
                                                            typedPath.isDirectory,
                                                            typedPath.isDirectory,
                                                            typedPath.isSymbolicLink)
        converter(path, simpleFileAttributes)
      }
    },
    true
  )
  private[this] val observers = new Observers[FileEvent[CustomFileAttributes[T]]]

  underlying.addCacheObserver(new CacheObserver[CustomFileAttributes[T]] {
    override def onCreate(newEntry: FileTreeDataViews.Entry[CustomFileAttributes[T]]): Unit = {
      val path = newEntry.getTypedPath.getPath
      newEntry.getValue.asScala.right.foreach { v =>
        observers.onNext(Creation(path, v))
      }
      ()
    }
    override def onDelete(oldEntry: FileTreeDataViews.Entry[CustomFileAttributes[T]]): Unit = {
      val path = oldEntry.getTypedPath.getPath
      oldEntry.getValue.asScala.right.foreach { v =>
        observers.onNext(Deletion(path, v))
      }
      ()
    }
    override def onUpdate(oldEntry: FileTreeDataViews.Entry[CustomFileAttributes[T]],
                          newEntry: FileTreeDataViews.Entry[CustomFileAttributes[T]]): Unit = {
      val path = newEntry.getTypedPath.getPath
      val oldEither = oldEntry.getValue.asScala
      val newEither = newEntry.getValue.asScala
      oldEither match {
        case Right(o) =>
          newEither match {
            case Right(n) => observers.onNext(Update(path, o, n))
            case _        => observers.onNext(Deletion(path, o))
          }
        case _ =>
          newEither match {
            case Right(n) => observers.onNext(Creation(path, n))
            case _        =>
          }
      }
    }
    override def onError(exception: IOException): Unit = {}
  }: CacheObserver[CustomFileAttributes[T]])
  override def addObserver(
      observer: Observer[FileEvent[CustomFileAttributes[T]]]): AutoCloseable = {
    throwIfClosed("addObserver")
    observers.addObserver(observer)
  }
  override def list(glob: Glob, filter: ((NioPath, CustomFileAttributes[T])) => Boolean)
    : Seq[(NioPath, CustomFileAttributes[T])] = {
    throwIfClosed("list")
    val res = new VectorBuilder[(NioPath, CustomFileAttributes[T])]
    underlying
      .listEntries(glob.base, glob.range.toSwovalDepth, Filters.AllPass)
      .iterator
      .asScala
      .foreach { e =>
        val tp = e.getTypedPath
        val path = tp.getPath
        e.getValue.asScala match {
          case Right(t: CustomFileAttributes[T] @unchecked) =>
            val pair = path -> t
            if (filter(pair)) res += pair
          case _ =>
        }
      }
    res.result
  }
  override def register(
      glob: Glob): Either[IOException, Observable[FileEvent[CustomFileAttributes[T]]]] = {
    throwIfClosed("register")
    underlying.register(glob.base, glob.range.toSwovalDepth).asScala match {
      case Right(_) => new RegisterableObservable(observers).register(glob)
      case Left(ex) => Left(ex)
    }
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
