package sbt.io

import java.io.IOException
import java.nio.file.LinkOption.NOFOLLOW_LINKS
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{ Files, Path => JPath }

import sbt.io.FileTreeDataView.{ Entry, Observable }

/**
 * Represents a path in the file system. It may cache some of the file attributes so that no disk
 * io is necessary to check, for example, whether the file is a directory or regular file.
 */
trait TypedPath {

  /**
   * The underlying path that this represents.
   * @return the path
   */
  def getPath: JPath

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

  override def toString: String = s"TypedPath($getPath)"
}

object TypedPath {
  def apply(path: JPath): TypedPath = new TypedPath {
    private val attrs = try {
      Some(Files.readAttributes(path, classOf[BasicFileAttributes], NOFOLLOW_LINKS))
    } catch {
      case _: IOException =>
        None
    }
    override def getPath: JPath = path
    override val exists: Boolean = attrs.isDefined
    override val isDirectory: Boolean = attrs.fold(false)(_.isDirectory)
    override val isFile: Boolean = attrs.fold(false)(_.isRegularFile)
    override val isSymbolicLink: Boolean = attrs.fold(false)(_.isSymbolicLink)
    override lazy val toRealPath: JPath = attrs
      .flatMap(a =>
        try {
          if (a.isSymbolicLink) Some(path.toRealPath()) else Some(path)
        } catch {
          case _: IOException => None
      })
      .getOrElse(path)
  }
}

/**
 * Provides a view into the file system that allows retrieval of the children of a particular path.
 * Specific implementations may or may not use a cache for retrieval.
 */
trait FileTreeView extends AutoCloseable {

  /**
   * List the contents of the current directory.
   *
   * @param path      the path to list
   * @param maxDepth  controls the depth of children of the path to include in the results. When
   *                  maxDepth is -1, [[list]] should only return the TypedPath for this directory.
   *                  For non-negative values, [[list]] should return only entries whose relativized
   *                  path has {{{maxDepth - 1}}} elements. For example, when maxDepth is zero, all of
   *                  the children of the path should be included in the result, but none of the
   *                  children of any of the subdirectories should be included.
   * @param filter    only return files accepted by the filter
   * @return a sequence of [[TypedPath]]s.
   */
  def list(path: JPath, maxDepth: Int, filter: TypedPath => Boolean): Seq[TypedPath]
}

object FileTreeView {
  object AllPass extends (Any => Boolean) {
    override def apply(e: Any): Boolean = true
  }
}

/**
 * Provides a view into the file system that allows retrieval of the children of a particular path
 * where each child has an associated [[FileTreeDataView.Entry]] that is generally derived from the
 * file (e.g. the [[FileTreeDataView.Entry.value]] may be the md5 hash of the underlying file).
 * Specific implementations may or may not use a cache for retrieval. It extends FileTreeView since
 * [[FileTreeView.list]] can be trivially implemented using `listEntries`.
 */
trait FileTreeDataView[+T] extends FileTreeView with AutoCloseable {

  /**
   * List the contents of the current directory where each returned [[FileTreeDataView.Entry]] has a
   * data value associated with it.
   *
   * @param path      the path to list
   * @param maxDepth  controls the depth of children of the path to include in the results. When
   *                  maxDepth is -1, [[listEntries]] should only return the TypedPath for this
   *                  directory. For non-negative values, [[listEntries]] should return only
   *                  entries whose relativized path has {{{maxDepth - 1}}} elements. For example,
   *                  when maxDepth is zero, all of the children of the path should be included in
   *                  the result, but none of the children of any of the subdirectories should be
   *                  included.
   * @param filter    only return files accepted by the filter
   * @return a sequence of [[FileTreeDataView.Entry]] instances.
   */
  def listEntries(path: JPath, maxDepth: Int, filter: Entry[T] => Boolean): Seq[Entry[T]]
}

object FileTreeDataView {
  abstract case class Entry[+T](path: JPath, value: Either[IOException, T]) extends TypedPath {
    override def toString: String = s"Entry($path, $value)"
  }

  /**
   * A FileRepository observer that receives callbacks
   * @tparam T the generic type of [[Entry.value]] instances for the [[FileRepository]]
   */
  trait Observer[-T] {

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
  object Observer {

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
  trait Observable[+T] extends AutoCloseable {

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
}

/**
 * Monitors registered directories for file changes. A typical implementation will keep an
 * in memory cache of the file system that can be queried in [[FileRepository#listEntries]]. The
 * [[FileRepository#register]] method adds monitoring for a particular cache. A filter may be provided
 * so that the cache doesn't waste memory on files the user doesn't care about. The
 * cache may be shared across a code base so there additional apis for adding filters or changing
 * the recursive property of a directory.
 *
 * @tparam T the type of the [[FileTreeDataView.Entry.value]]s.
 */
trait FileRepository[+T] extends Observable[T] with FileTreeDataView[T] with AutoCloseable {

  /**
   * Register a directory for monitoring
   *
   * @param path     the path to list
   * @param maxDepth controls how the depth of children of the registered path to consider. When
   *                 maxDepth is -1, then the repository should only monitor the path itself. When
   *                 it is zero, the the repository should monitor the path and its direct children.
   *                 For values greater than zero,
   * @return an Either that is a Right when register has no errors and a Left if an IOException is
   *         thrown while registering the path. The result should be true if the path has
   *         never been previously registered or if the recursive flag flips from false to true.
   */
  def register(path: JPath, maxDepth: Int): Either[IOException, Boolean]

  /**
   * Remove a path from monitoring.
   * @param path the path to stop monitoring
   */
  def unregister(path: JPath): Unit
}
