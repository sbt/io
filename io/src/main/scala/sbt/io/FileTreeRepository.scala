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

import java.io.{ File, IOException }
import java.nio.file.LinkOption.NOFOLLOW_LINKS
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{ Files, NoSuchFileException, Path => JPath }
import java.util
import java.util.concurrent.atomic.AtomicInteger

import sbt.internal.io.{
  DefaultFileTreeView,
  FileTreeRepositoryImpl,
  HybridPollingFileTreeRepository,
  LegacyFileTreeRepository
}
import sbt.io.FileTreeDataView.{ Entry, Observable }

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
 * Represents a path in the file system. It may cache some of the file attributes so that no disk
 * io is necessary to check, for example, whether the file is a directory or regular file.
 */
private[sbt] trait TypedPath {

  /**
   * The underlying path that this represents.
   * @return the path
   */
  def toPath: JPath

  /**
   * Indicates whether or not the file exists. Because it may be cached, this method may not
   * represent the current state of the file.
   * @return true if the file exists
   */
  def exists: Boolean

  /**
   * Indicates whether or not the file is a directory. Because it may be cached, this method may not
   * represent the current state of the file.
   * @return true if the file is a directory
   */
  def isDirectory: Boolean

  /**
   * Indicates whether or not the file is a regular file. Because it may be cached, this method may not
   * represent the current state of the file.
   * @return true if the file is a regular file
   */
  def isFile: Boolean

  /**
   * Indicates whether or not the file is a symbolic link. Because it may be cached, this method may not
   * represent the current state of the file.
   * @return true if the file is a symbolic link
   */
  def isSymbolicLink: Boolean
}

private[sbt] object TypedPath {
  def apply(path: JPath): TypedPath = new TypedPath with MetaTypedPath {
    private val attrs = try {
      Some(Files.readAttributes(path, classOf[BasicFileAttributes], NOFOLLOW_LINKS))
    } catch {
      case _: IOException =>
        None
    }
    override def toPath: JPath = path
    override val exists: Boolean = attrs.isDefined
    override val isDirectory: Boolean = attrs.fold(false)(_.isDirectory)
    override val isFile: Boolean = attrs.fold(false)(_.isRegularFile)
    override val isSymbolicLink: Boolean = attrs.fold(false)(_.isSymbolicLink)
  }
  implicit case object ordering extends Ordering[TypedPath] {
    override def compare(left: TypedPath, right: TypedPath): Int =
      left.toPath.compareTo(right.toPath)
  }

  /**
   * Provides extension methods for [[TypedPath]].
   * @param typedPath the [[TypedPath]] to extend
   */
  implicit class Ops(val typedPath: TypedPath) extends AnyVal {

    /**
     * View the [[TypedPath]] as a java.io.File. The `isFile` and `isDirectory` methods read
     * from the backing [[TypedPath]] to avoid unnecessary io.
     * @return the java.io.File instance.
     */
    def asFile: File = new File(typedPath.toPath.toString) {
      override def isFile: Boolean = typedPath.isFile
      override def isDirectory: Boolean = typedPath.isDirectory
    }
  }

  private[sbt] trait MetaTypedPath { self: TypedPath =>
    private[this] def toHash(boolean: Boolean) = if (boolean) 1 else 0
    override def hashCode: Int =
      ((((self.toPath.## * 31) ^ toHash(self.exists)) * 31 ^ toHash(self.isDirectory)) * 31 ^ toHash(
        self.isFile)) * 31 ^ toHash(self.isSymbolicLink)
    override def equals(o: Any): Boolean = o match {
      case tp: TypedPath =>
        self.toPath == tp.toPath && self.exists == tp.exists && self.isFile == tp.isFile &&
          self.isDirectory == tp.isDirectory && self.isSymbolicLink == tp.isSymbolicLink
      case _ => false
    }
    override def toString: String = s"TypedPath(${self.toPath})"
  }
}

/**
 * Provides a view into the file system that allows retrieval of the children of a particular path.
 * Specific implementations may or may not use a cache for retrieval.
 */
private[sbt] trait FileTreeView extends AutoCloseable {

  /**
   * List the contents of the current directory.
   *
   * @param glob The glob to list
   * @return a sequence of [[TypedPath]]s.
   */
  def list(glob: Glob): Seq[TypedPath]
}

private[sbt] object FileTreeView {
  object AllPass extends (Any => Boolean) {
    override def apply(e: Any): Boolean = true
  }
  val DEFAULT: FileTreeView = DefaultFileTreeView
  private class FileTreeDataViewFromFileTreeView[+T](view: FileTreeView, converter: TypedPath => T)
      extends FileTreeDataView[T] {
    override def listEntries(glob: Glob): Seq[Entry[T]] =
      list(glob).flatMap(tp => Some(Entry(tp, Entry.converter(converter))))
    override def list(glob: Glob): Seq[TypedPath] =
      try {
        view.list(glob)
      } catch {
        case _: NoSuchFileException => Nil
      }

    override def close(): Unit = view.close()
  }

  /**
   * Provides extension methods for [[FileTreeView]].
   * @param fileTreeView the [[FileTreeView]] instance to augment
   */
  implicit class Ops(val fileTreeView: FileTreeView) extends AnyVal {

    /**
     * Lift the [[FileTreeView]] to a [[FileTreeDataView]] given a conversion function from
     * [[TypedPath]] to `T` for some `T`. It works by delegating to the [[FileTreeView.list]]
     * method and transforming the results using the conversion function.
     * @param converter converts [[TypedPath]] to some generic type `T`.
     * @tparam T the generic type of cache values for the [[FileTreeDataView.Entry]] instances
     * @return the [[FileTreeDataView]] for
     */
    def asDataView[T](converter: TypedPath => T): FileTreeDataView[T] = {
      // scala 2.10 wouldn't compile when this was implemented with an anonymous class
      new FileTreeDataViewFromFileTreeView[T](fileTreeView, converter)
    }
  }
}

/**
 * Provides a view into the file system that allows retrieval of the children of a particular path
 * where each child has an associated [[FileTreeDataView.Entry]] that is generally derived from the
 * file (e.g. the [[FileTreeDataView.Entry.value]] may be the md5 hash of the underlying file).
 * Specific implementations may or may not use a cache for retrieval. It extends FileTreeView since
 * [[FileTreeView.list]] can be trivially implemented using `listEntries`.
 */
private[sbt] trait FileTreeDataView[+T] extends FileTreeView with AutoCloseable {

  /**
   * List the contents of the current directory where each returned [[FileTreeDataView.Entry]] has a
   * data value associated with it.
   *
   * @param glob      the glob to list
   * @return a sequence of [[FileTreeDataView.Entry]] instances.
   */
  def listEntries(glob: Glob): Seq[Entry[T]]
}

private[sbt] object FileTreeDataView {
  private[sbt] final case class Entry[+T](typedPath: TypedPath, value: Either[IOException, T]) {
    override def toString: String = s"Entry(${typedPath.toPath}, $value)"
  }

  /**
   * A FileTreeRepository observer that receives callbacks
   *
   * @tparam T the generic type of [[Entry.value]] instances for the [[FileTreeRepository]]
   */
  private[sbt] trait Observer[-T] {

    /**
     * Process a newly created entry
     * @param newEntry the newly acceptcreate entry
     */
    def onCreate(newEntry: Entry[T]): Unit

    /**
     * Process a deleted entry
     * @param oldEntry the deleted entry
     */
    def onDelete(oldEntry: Entry[T]): Unit

    /**
     * Process a deleted entry
     * @param oldEntry the previous entry
     * @param newEntry the current entry
     */
    /*
     * Note that this interface exists for documentation purposes so that it is clear that the
     * deleted entry is on the left. This would not be clear if filtering was handled with a
     * `(Entry[T], Entry[T]) => Boolean`.
     */
    def onUpdate(oldEntry: Entry[T], newEntry: Entry[T]): Unit
  }
  private[sbt] object Observer {

    /**
     * Create a new Observer from callback functions.
     *
     * @param onCreate the callback to invoke when a new entry is created
     * @param onDelete the callback to invoke when an entry is
     * @param onUpdate the callback to invoke when an entry is updated. Note that the previous
     *                 entry must be the first argument to this function.
     * @tparam T the type of [[Entry.value]] instances for the [[Observable]]
     * @return an [[Observer]] with the provided callbacks.
     */
    def apply[T](onCreate: Entry[T] => Unit,
                 onDelete: Entry[T] => Unit,
                 onUpdate: (Entry[T], Entry[T]) => Unit): Observer[T] =
      new Impl(onCreate, onDelete, onUpdate)

    private[FileTreeDataView] class Impl[-T](oncreate: Entry[T] => Unit,
                                             ondelete: Entry[T] => Unit,
                                             onupdate: (Entry[T], Entry[T]) => Unit)
        extends Observer[T] {
      override def onCreate(newEntry: Entry[T]): Unit = oncreate(newEntry)
      override def onDelete(oldEntry: Entry[T]): Unit = ondelete(oldEntry)
      override def onUpdate(oldEntry: Entry[T], newEntry: Entry[T]): Unit =
        onupdate(oldEntry, newEntry)
      override def toString: String =
        s"Observer(onCreate = $oncreate, onDelete = $ondelete, onUpdate = $onupdate)"
    }
  }

  /**
   * An object that monitors a file system. The interface is very similar to that provided by other
   * libraries/frameworks, such as [[http://reactivex.io/intro.html rxJava]]. When it detects changes
   * in the file system, it will invoke a set of user specified callbacks. The Observable also
   * allows the user to add and removes paths to monitor.
   *
   * @tparam T the generic type of [[Entry.value]] instances
   */
  private[sbt] trait Observable[+T] extends AutoCloseable {

    /**
     * Add callbacks to be invoked on file events.
     *
     * @param observer the callbacks to invoke
     * @return a handle to the callback.
     */
    def addObserver(observer: Observer[T]): Int

    /**
     * Removes a callback that was added via [[addObserver]]
     *
     * @param handle The handle returned by [[addObserver]]
     */
    def removeObserver(handle: Int): Unit
  }

  private[sbt] object Entry {

    def converter[T](f: TypedPath => T): TypedPath => Either[IOException, T] =
      (typedPath: TypedPath) =>
        try {
          Right(f(typedPath))
        } catch {
          case e: IOException => Left(e)
      }
    def apply[T](typedPath: TypedPath, converter: TypedPath => Either[IOException, T]): Entry[T] =
      Entry(typedPath, converter(typedPath))
  }

  /**
   * Aggregates a collection of [[Observer]]s into a single [[Observer]]. The callbacks for the
   * generated [[Observer]] invoke the corresponding callback for each of the [[Observer]]s that
   * are added via [[addObserver]].
   *
   * @tparam T the generic type of [[Entry.value]] instances for the [[FileTreeRepository]]
   */
  private[sbt] class Observers[T] extends Observer[T] with Observable[T] {
    private[this] val id = new AtomicInteger(0)
    private[this] val observers: mutable.Map[Int, Observer[T]] =
      new util.LinkedHashMap[Int, Observer[T]]().asScala

    override def onCreate(newEntry: Entry[T]): Unit = observers.synchronized {
      observers.values.foreach(_.onCreate(newEntry))
    }

    override def onDelete(oldEntry: Entry[T]): Unit = observers.synchronized {
      observers.values.foreach(_.onDelete(oldEntry))
    }

    override def onUpdate(oldEntry: Entry[T], newEntry: Entry[T]): Unit = observers.synchronized {
      observers.values.foreach(_.onUpdate(oldEntry, newEntry))
    }

    override def addObserver(observer: Observer[T]) = observers.synchronized {
      val observerId = id.incrementAndGet()
      observers += observerId -> observer
      observerId
    }

    override def removeObserver(handle: Int): Unit = observers.synchronized {
      observers -= handle
      ()
    }

    override def close(): Unit = observers.synchronized(observers.clear())
    override def toString: String =
      s"Observers(\n${observers.map { case (k, v) => s"  $k -> $v" }.mkString("\n")}\n)"
  }

  implicit class CallbackOps[-T](val callback: Entry[T] => Unit)
      extends Observer.Impl[T](callback(_: Entry[T]),
                               callback(_: Entry[T]),
                               (_: Entry[T], newEntry: Entry[T]) => callback(newEntry))

}

/**
 * A dynamically configured monitor of the file system. New paths can be added and removed from
 * monitoring with register / unregister.
 */
private[sbt] trait Registerable {

  /**
   * Register a glob for monitoring.
   *
   * @param glob Glob
   * @return an Either that is a Right when register has no errors and a Left if an IOException is
   *         thrown while registering the path. The result should be true if the path has
   *         never been previously registered or if the recursive flag flips from false to true.
   */
  def register(glob: Glob): Either[IOException, Boolean]

  /**
   * Remove a path from monitoring.
   * @param glob the glob to stop monitoring
   */
  def unregister(glob: Glob): Unit
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
 * @tparam T the type of the [[FileTreeDataView.Entry.value]]s.
 */
private[sbt] trait FileTreeRepository[+T]
    extends Registerable
    with Observable[T]
    with FileTreeDataView[T]
    with AutoCloseable

private[sbt] object FileTreeRepository {

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for the
   * monitored directories.
   *
   * @param converter function to generate an [[FileTreeDataView.Entry.value]] from a [[TypedPath]]
   * @tparam T the generic type of the [[FileTreeDataView.Entry.value]]
   * @return a file repository.
   */
  private[sbt] def default[T](converter: TypedPath => T): FileTreeRepository[T] =
    new FileTreeRepositoryImpl[T](converter)

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for the
   * monitored directories.
   *
   * @param converter function to generate an [[FileTreeDataView.Entry.value]] from a [[TypedPath]]
   * @tparam T the generic type of the [[FileTreeDataView.Entry.value]]
   * @return a file repository.
   */
  private[sbt] def legacy[T](converter: TypedPath => T): FileTreeRepository[T] =
    new LegacyFileTreeRepository[T](converter, new WatchLogger {
      override def debug(msg: => Any): Unit = {}
    }, WatchService.default)

  /**
   * Create a [[FileTreeRepository]] with a provided logger. The generated repository will cache
   * the file system tree for the monitored directories.
   *
   * @param converter function to generate an [[FileTreeDataView.Entry.value]] from a [[TypedPath]]
   * @param logger used to log file events
   * @param watchService the [[WatchService]] to monitor for file system events
   * @tparam T the generic type of the [[FileTreeDataView.Entry.value]]
   * @return a file repository.
   */
  private[sbt] def legacy[T](converter: TypedPath => T,
                             logger: WatchLogger,
                             watchService: WatchService): FileTreeRepository[T] =
    new LegacyFileTreeRepository[T](converter, logger, watchService)

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for some
   * of the paths under monitoring, but others will need to be polled.
   *
   * @param converter function to generate an [[FileTreeDataView.Entry.value]] from a [[TypedPath]]
   * @param pollingGlobs do not cache any path contained in these [[Glob]]s.
   * @tparam T the generic type of the [[FileTreeDataView.Entry.value]]
   * @return a file repository.
   */
  private[sbt] def hybrid[T](converter: TypedPath => T,
                             pollingGlobs: Glob*): HybridPollingFileTreeRepository[T] =
    HybridPollingFileTreeRepository(converter, pollingGlobs: _*)
}
