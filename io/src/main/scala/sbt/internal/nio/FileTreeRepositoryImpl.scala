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
import java.nio.file.{ Path => NioPath }
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import com.swoval.files.FileTreeDataViews.CacheObserver
import com.swoval.files.{ FileTreeDataViews, FileTreeRepositories, TypedPath => STypedPath }
import com.swoval.functional.Filters
import sbt.internal.nio.FileEvent.{ Creation, Deletion, Update }
import sbt.internal.nio.SwovalConverters._
import sbt.nio.file.{ FileAttributes, Glob }

import scala.collection.JavaConverters._
import scala.collection.immutable.VectorBuilder
import scala.util.Properties

/**
 * The default implemenation of [[FileTreeRepository]]. It delegates all of its methods to the
 * [[https://swoval.github.io/files/jvm/com/swoval/files/FileTreeRepository.html swoval FileTreeRepository]].
 *
 * @tparam T the type of the values.
 */
private[sbt] class FileTreeRepositoryImpl[T] extends FileTreeRepository[FileAttributes] {
  private[this] val closed = new AtomicBoolean(false)
  private[this] val underlying = FileTreeRepositories.get[FileAttributes](
    (typedPath: STypedPath) => {
      FileAttributes(
        isDirectory = typedPath.isDirectory,
        isOther = false,
        isRegularFile = typedPath.isFile,
        isSymbolicLink = typedPath.isSymbolicLink
      )
    },
    true
  )
  private[this] val observers = new Observers[FileEvent[FileAttributes]]
  private[this] val registered = ConcurrentHashMap.newKeySet[NioPath].asScala
  private[this] val isMac = Properties.isMac

  underlying.addCacheObserver(new CacheObserver[FileAttributes] {
    override def onCreate(newEntry: FileTreeDataViews.Entry[FileAttributes]): Unit = {
      val path = newEntry.getTypedPath.getPath
      newEntry.getValue.asScala.right.foreach { v =>
        observers.onNext(Creation(path, v))
      }
      ()
    }
    override def onDelete(oldEntry: FileTreeDataViews.Entry[FileAttributes]): Unit = {
      val path = oldEntry.getTypedPath.getPath
      oldEntry.getValue.asScala.right.foreach { v =>
        observers.onNext(Deletion(path, v))
      }
      ()
    }
    override def onUpdate(
        oldEntry: FileTreeDataViews.Entry[FileAttributes],
        newEntry: FileTreeDataViews.Entry[FileAttributes]
    ): Unit = {
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
  }: CacheObserver[FileAttributes])
  override def addObserver(observer: Observer[FileEvent[FileAttributes]]): AutoCloseable = {
    throwIfClosed("addObserver")
    observers.addObserver(observer)
  }
  override def list(path: NioPath): Seq[(NioPath, FileAttributes)] = {
    throwIfClosed("list")
    val res = new VectorBuilder[(NioPath, FileAttributes)]
    underlying
      .listEntries(path, 0, Filters.AllPass)
      .iterator
      .asScala
      .foreach { e =>
        val tp = e.getTypedPath
        val path = tp.getPath
        e.getValue.asScala match {
          case Right(t: FileAttributes @unchecked) => res += path -> t
          case _                                   =>
        }
      }
    res.result
  }
  override def register(glob: Glob): Either[IOException, Observable[FileEvent[FileAttributes]]] = {
    throwIfClosed("register")
    val base = glob.base
    if (isMac) {
      // workaround for https://github.com/sbt/sbt/issues/4603
      val parent = glob.base.getParent
      if (!registered.contains(parent)) {
        if (registered.exists(
              path =>
                path.getParent == parent && {
                  val leftFileName = path.getFileName.toString
                  val rightFileName = base.getFileName.toString
                  leftFileName != rightFileName && (leftFileName
                    .startsWith(rightFileName) || rightFileName.startsWith(leftFileName))
                }
            )) {
          register(Glob(parent))
        }
      }
    }
    underlying.register(base, glob.range.toSwovalDepth).asScala match {
      case Right(_) =>
        registered.add(base)
        new RegisterableObservable(observers).register(glob)
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
