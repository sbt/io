package sbt.io

import java.io.{ File, IOException }
import java.nio.file.LinkOption.NOFOLLOW_LINKS
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{ Files, NoSuchFileException, Path => JPath }

import sbt.internal.io.{
  DefaultFileTreeView,
  FileTreeRepositoryImpl,
  HybridPollingFileTreeRepository,
  Source
}
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

  override def toString: String = s"TypedPath($toPath)"
}

object TypedPath {
  def apply(path: JPath): TypedPath = new TypedPath {
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
  val DEFAULT: FileTreeView = DefaultFileTreeView
  private class FileTreeDataViewFromFileTreeView[+T](view: FileTreeView, converter: TypedPath => T)
      extends FileTreeDataView[T] {
    override def listEntries(path: JPath,
                             maxDepth: Int,
                             filter: Entry[T] => Boolean): Seq[Entry[T]] =
      list(path, maxDepth, (_: TypedPath) => true)
        .flatMap(tp => Some(Entry(tp, Entry.converter(converter))).filter(filter))
    override def list(path: JPath, maxDepth: Int, filter: TypedPath => Boolean): Seq[TypedPath] =
      try {
        view.list(path, maxDepth, filter)
      } catch {
        case _: NoSuchFileException => Nil
      }

    override def close(): Unit = view.close()
  }
  implicit class FileTreeViewOps(val fileTreeView: FileTreeView) extends AnyVal {
    // scala 2.10 wouldn't compile when this was implemented with an anonymous class
    def asDataView[T](f: TypedPath => T): FileTreeDataView[T] =
      new FileTreeDataViewFromFileTreeView[T](fileTreeView, f)
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
  final case class Entry[+T](typedPath: TypedPath, value: Either[IOException, T]) {
    override def toString: String = s"Entry(${typedPath.toPath}, $value)"
  }
  implicit class Ops[T](val view: FileTreeDataView[T]) extends AnyVal {
    def list(glob: Glob): Seq[TypedPath] =
      view.list(glob.base, glob.depth, glob.toTypedPathFilter)
    def listEntries(glob: Glob): Seq[FileTreeDataView.Entry[T]] =
      view.listEntries(glob.base, glob.depth, glob.toEntryFilter)
  }

  /**
   * A FileTreeRepository observer that receives callbacks
   *
   * @tparam T the generic type of [[Entry.value]] instances for the [[FileTreeRepository]]
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

  object Entry {

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

  implicit class CallbackOps[-T](val callback: Entry[T] => Unit)
      extends Observer.Impl[T](callback(_: Entry[T]),
                               callback(_: Entry[T]),
                               (_: Entry[T], newEntry: Entry[T]) => callback(newEntry))

}

/**
 * Monitors registered directories for file changes. A typical implementation will keep an
 * in memory cache of the file system that can be queried in [[FileTreeRepository#listEntries]]. The
 * [[FileTreeRepository#register]] method adds monitoring for a particular cache. A filter may be provided
 * so that the cache doesn't waste memory on files the user doesn't care about. The
 * cache may be shared across a code base so there additional apis for adding filters or changing
 * the recursive property of a directory.
 *
 * @tparam T the type of the [[FileTreeDataView.Entry.value]]s.
 */
trait FileTreeRepository[+T] extends Observable[T] with FileTreeDataView[T] with AutoCloseable {

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

object FileTreeRepository {

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for the
   * monitored directories.
   *
   * @param converter function to generate an [[FileTreeDataView.Entry.value]] from a [[TypedPath]]
   * @tparam T the generic type of the [[FileTreeDataView.Entry.value]]
   * @return a file repository.
   */
  def default[T](converter: TypedPath => T): FileTreeRepository[T] =
    new FileTreeRepositoryImpl[T](converter)

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for some
   * of the paths under monitoring, but others will need to be polled.
   *
   * @param converter function to generate an [[FileTreeDataView.Entry.value]] from a [[TypedPath]]
   * @param pollingSources do not cache any path contained in these [[sbt.internal.io.Source]]s.
   * @tparam T the generic type of the [[FileTreeDataView.Entry.value]]
   * @return a file repository.
   */
  def hybrid[T](converter: TypedPath => T,
                pollingSources: Source*): HybridPollingFileTreeRepository[T] =
    HybridPollingFileTreeRepository(converter, pollingSources: _*)
}
