package sbt.nio

import java.nio.file.Path

import sbt.internal.nio.DefaultFileTreeView

/**
 * Provides a view into the file system that allows retrieval of the children of a particular path.
 * Specific implementations may or may not use a cache for retrieval.
 * @tparam T the type of object returned for each file
 */
trait FileTreeView[+T] {

  /**
   * List the contents of the current directory.
   *
   * @param glob the files to include
   * @return a sequence of values corresponding to each path described by the glob
   */
  def list(glob: Glob): Seq[T]
}
private[sbt] object FileTreeView {
  private[sbt] type Nio[+T] = FileTreeView[(Path, T)]
  private[sbt] val DEFAULT_NIO: Nio[FileAttributes] = DefaultFileTreeView
  private[sbt] implicit class NioFileTreeViewOps[T](val view: FileTreeView.Nio[T]) {
    def map[A >: T, B](f: (Path, A) => B): FileTreeView.Nio[B] = {
      val converter: ((Path, A)) => (Path, B) = {
        case (path: Path, attrs) => path -> f(path, attrs)
      }
      (glob: Glob) =>
        view.list(glob).map(converter)
    }
    def flatMap[B, A >: T](f: (Path, A) => Traversable[B]): FileTreeView.Nio[B] = {
      val converter: ((Path, A)) => Traversable[(Path, B)] = {
        case (path: Path, attrs) => f(path, attrs).map(path -> _)
      }
      (glob: Glob) =>
        view.list(glob).flatMap(converter(_))
    }
  }
}
