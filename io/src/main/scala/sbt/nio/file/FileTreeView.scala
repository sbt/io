package sbt.nio.file

import java.nio.file.Path

import sbt.internal.nio.DefaultFileTreeView

/**
 * Provides a view into the file system that allows retrieval of the children of a particular path.
 * Specific implementations may or may not use a cache for retrieval.
 * @tparam T the type of object returned for each file
 */
trait FileTreeView[+T] {

  /**
   * List the contents of a current directory.
   *
   * @param path the directory to list
   * @return a sequence of values corresponding to each path that is a direct child of the input
   *         path.
   */
  def list(path: Path): Seq[T]
}
object FileTreeView {

  /**
   * Adds additional methods to [[FileTreeView]]. This api may be changed so it should not be
   * imported directly.
   * @param fileTreeView the [[FileTreeView]] to augment.
   */
  implicit class Ops(val fileTreeView: FileTreeView.Nio[FileAttributes]) extends AnyVal {
    def list(glob: Glob): Seq[(Path, FileAttributes)] = Glob.all(glob :: Nil, fileTreeView)
    def list(globs: Traversable[Glob]): Seq[(Path, FileAttributes)] = Glob.all(globs, fileTreeView)
  }
  private[sbt] type Nio[+T] = FileTreeView[(Path, T)]
  def default: FileTreeView[(Path, FileAttributes)] = DEFAULT_NIO
  private[this] val DEFAULT_NIO: Nio[FileAttributes] = DefaultFileTreeView
  private[sbt] implicit class NioFileTreeViewOps[T](val view: FileTreeView.Nio[T]) {
    def map[A >: T, B](f: (Path, A) => B): FileTreeView.Nio[B] = {
      val converter: ((Path, A)) => (Path, B) = {
        case (path: Path, attrs) => path -> f(path, attrs)
      }
      (path: Path) =>
        view.list(path).map(converter)
    }
    def flatMap[B, A >: T](f: (Path, A) => Traversable[B]): FileTreeView.Nio[B] = {
      val converter: ((Path, A)) => Traversable[(Path, B)] = {
        case (path: Path, attrs) => f(path, attrs).map(path -> _)
      }
      (path: Path) =>
        view.list(path).flatMap(converter(_))
    }
  }
}
