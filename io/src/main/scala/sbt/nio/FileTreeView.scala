package sbt.nio

import java.nio.file.Path

import sbt.internal.nio.{ DefaultFileTreeView, NioFileTreeView }

/**
 * Provides a view into the file system that allows retrieval of the children of a particular path.
 * Specific implementations may or may not use a cache for retrieval.
 * @tparam T the type of object returned for each file
 */
trait FileTreeView[+T] extends AutoCloseable {

  /**
   * List the contents of the current directory.
   *
   * @param glob the files to include
   * @return a sequence of values corresponding to each path described by the glob
   */
  def list(glob: Glob, filter: T => Boolean): Seq[T]

  // Many, if not most, FileTreeViews should not create new resources.
  override def close(): Unit = {}
}
private[sbt] object FileTreeView {
  private[sbt] type Nio[T] = FileTreeView[(Path, T)]
  private[sbt] type Io[T] = FileTreeView[(java.io.File, T)]
  private[sbt] val DEFAULT_NIO: Nio[FileAttributes] = DefaultFileTreeView
  private[sbt] val DEFAULT_IO: Io[FileAttributes] = new MappedFileTreeView(DefaultFileTreeView, {
    case (p: Path, a: FileAttributes) => p.toFile -> a
  }: ((Path, FileAttributes)) => (java.io.File, FileAttributes), closeUnderlying = true)
  private class MappedFileTreeView[+T, +R](view: FileTreeView[T],
                                           converter: T => R,
                                           closeUnderlying: Boolean)
      extends FileTreeView[R] {
    override def list(glob: Glob, filter: R => Boolean): Seq[R] = {
      view.list(glob, _ => true).flatMap { t =>
        val r: R = converter(t)
        if (filter(r)) r :: Nil else Nil
      }
    }
    override def close(): Unit = if (closeUnderlying) view.close()
  }
  private[sbt] implicit class NioFileTreeViewOps[T](val view: FileTreeView.Nio[T]) {
    def map[A >: T, B](f: (Path, A) => B): NioFileTreeView[B] = {
      val mapped: FileTreeView[(Path, B)] = {
        val converter: ((Path, A)) => (Path, B) = {
          case (path: Path, attrs) => path -> f(path, attrs)
        }
        new MappedFileTreeView(view, converter, closeUnderlying = true)
      }
      (glob: Glob, filter: ((Path, B)) => Boolean) =>
        mapped.list(glob, filter)
    }
    def flatMap[B, A >: T](f: (Path, A) => Traversable[B]): NioFileTreeView[B] = {
      val converter: ((Path, A)) => Traversable[(Path, B)] = {
        case (path: Path, attrs) => f(path, attrs).map(path -> _)
      }
      (glob: Glob, filter: ((Path, B)) => Boolean) =>
        view.list(glob, _ => true).flatMap(converter(_).filter(filter))
    }
  }
}
