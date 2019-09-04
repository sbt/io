/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.nio.file

import java.io.IOException
import java.nio.file.{ Files, NoSuchFileException, NotDirectoryException, Path }
import java.util
import java.util.concurrent.ConcurrentHashMap

import sbt.internal.io.Retry
import sbt.internal.nio.SwovalFileTreeView
import sbt.nio.file.Glob.Root

import scala.annotation.tailrec
import scala.collection.JavaConverters._

/**
 * Provides a view into the file system that allows retrieval of the children of a particular path.
 * Specific implementations may use native library on some platforms to speed up recursive
 * file tree traversal.
 * @tparam T the type of object returned for each file
 */
trait FileTreeView[+T] {

  /**
   * List the contents of a current directory.
   *
   * @param path the directory to list
   * @return a sequence of values corresponding to each path that is a direct child of the input
   *         path. The implementation may throw an IOException if the input path is not a Directory
   *         or
   */
  def list(path: Path): Seq[T]
}
object FileTreeView {

  /**
   * An implementation of [[FileTreeView]] that uses the swoval library, which provides native
   * apis for faster directory traversal on 64 bit Linux, Mac OS and Windows. This implementation
   * will throw an IOException if the input path is not a directory or doesn't exist.
   */
  val native: FileTreeView.Nio[FileAttributes] = SwovalFileTreeView

  /**
   * An implementation of [[FileTreeView]] that uses built in jvm apis. This implementation
   * will throw an IOException if the input path is not a directory or doesn't exist.
   */
  val nio: FileTreeView.Nio[FileAttributes] = (path: Path) =>
    Retry(
      {
        val stream = Files.list(path)
        try stream.iterator.asScala.flatMap(p => FileAttributes(p).toOption.map(p -> _)).toVector
        finally stream.close()
      },
      classOf[NotDirectoryException],
      classOf[NoSuchFileException]
    )

  /**
   * Adds additional methods to [[FileTreeView]]. This api may be changed so it should not be
   * imported directly.
   * @param fileTreeView the [[FileTreeView]] to augment.
   */
  implicit class Ops(val fileTreeView: FileTreeView.Nio[FileAttributes]) extends AnyVal {

    /**
     * Returns all of the existing paths on the file system that match the [[Glob]] pattern. It
     * should not throw an IOException and will return an empty sequence if no paths exist that
     * match the pattern. It will print a warning if the glob does not have an absolute
     * base path but it will expand the glob to `Paths.get("").toAbsolutePath.toGlob / glob`
     * for traversal.
     *
     * @param glob the search query
     * @return all of the paths that match the search query.
     */
    def list(glob: Glob): Seq[(Path, FileAttributes)] = all(glob :: Nil, fileTreeView)

    /**
     * Returns a filtered list of the existing paths on the file system that match the [[Glob]]
     * pattern. It should not throw an IOException and will return an empty sequence if no paths
     * exist that match the patterns and filter. It will print a warning if any of the globs do not
     * have an absolute base path but it will expand the glob to `Paths.get("").toGlob / glob` for
     * traversal.
     *
     * @param glob the search query
     * @param filter the filter for the path name and attributes of each file in the result set
     * @return all of the paths that match the search query.
     */
    def list(glob: Glob, filter: PathFilter): Seq[(Path, FileAttributes)] =
      all(glob :: Nil, fileTreeView, filter)

    /**
     * Returns all of the existing paths on the file system that match the [[Glob]] pattern. This
     * method should not throw and will return an empty sequence if no paths exist that match
     * the patterns. It will print a warning if any of the globs do not have an absolute
     * base path but it will expand the glob to `Paths.get("").toGlob / glob` for traversal. It
     * optimizes traversal so that each directory on the file system is only listed once:
     *
     * {{{
     *   val dir = Paths.get("").toAbsolutePath.toGlob
     *   // This only lists the current directory once
     *   val view = FileTreeView.default
     *   val sources = view.list(Seq(dir / "*.scala", dir / "*.java"))
     *   // This lists the current directory twice
     *   val slowSources = view.list(dir / "*.scala") ++ view.list(current / "*.java")
     * }}}
     *
     * @param globs the search queries
     * @return all of the paths that match the search query.
     */
    def list(globs: Traversable[Glob]): Seq[(Path, FileAttributes)] = all(globs, fileTreeView)

    /**
     * Returns a filtered list of the existing paths on the file system that match the [[Glob]]
     * pattern. It should not throw an IOException and will return an empty sequence if no paths
     * exist that match the patterns and filter. It will print a warning if any of the globs do not
     * have an absolute base path but it will expand the glob to `Paths.get("").toGlob / glob` for
     * traversal. It optimizes traversal so that each directory on the file system is only listed
     * once:
     *
     * {{{
     *   val dir = Paths.get("").toAbsolutePath.toGlob
     *   // This only lists the current directory once
     *   val view = FileTreeView.default
     *   val filter: (Path, FileAttributes) = (_, a) => a.isRegularFile
     *   val sources = view.list(Seq(dir / "*.scala", dir / "*.java"), filter)
     *   // This lists the current directory twice
     *   val slowSources =
     *     view.list(dir / "*.scala", filter) ++ view.list(current / "*.java", filter)
     * }}}
     *
     * @param globs the search queries
     * @param filter the filter for the path name and attributes of each file in the result set
     * @return all of the paths that match the search query.
     */
    def list(globs: Traversable[Glob], filter: PathFilter): Seq[(Path, FileAttributes)] =
      all(globs, fileTreeView, filter)

    /**
     * Returns an iterator for all of the existing paths on the file system that match the [[Glob]]
     * pattern. It should not throw an IOException and will return an empty sequence if no paths
     * exist that match the pattern. It will print a warning if the glob does not have an absolute
     * base path but it will expand the glob to `Paths.get("").toAbsolutePath.toGlob / glob`
     * for traversal.
     *
     * @param glob the search query
     * @return an iterator for all of the paths that match the search query.
     */
    def iterator(glob: Glob): Iterator[(Path, FileAttributes)] =
      FileTreeView.iterator(glob :: Nil, fileTreeView)

    /**
     * Returns a filtered iterator for all of the existing paths on the file system that match the
     * [[Glob]] pattern. It should not throw an IOException and will return an empty sequence if no
     * paths exist that match the pattern and the filter. It will print a warning if the glob does
     * not have an absolute base path but it will expand the glob to
     * `Paths.get("").toAbsolutePath.toGlob / glob` for traversal.
     *
     * {{{
     *   val dir = Paths.get("").toAbsolutePath.toGlob
     *   val regularNonHiddenFiles = FileTreeView.default.list(dir / "*.scala",
     *    (p: Path, a: FileAttributes) => a.isRegularFile && !Files.isHidden(p) )
     * }}}
     *
     * @param glob the search query
     * @param filter the filter for the path name and attributes of each file in the result set
     * @return an iterator for all of the paths that match the search query.
     */
    def iterator(glob: Glob, filter: PathFilter): Iterator[(Path, FileAttributes)] =
      FileTreeView.iterator(glob :: Nil, fileTreeView, filter)

    /**
     * Returns all of the existing paths on the file system that match the [[Glob]] pattern. It
     * should not throw an IOException and will return an empty sequence if no paths exist tha
     * match the patterns. It will print a warning if any of the globs do not have an absolute
     * base path but it will expand the glob to `Paths.get("").toGlob / glob` for traversal. It
     * optimizes traversal so that each directory on the file system is only listed once:
     *
     * {{{
     *   val dir = Paths.get("").toAbsolutePath.toGlob
     *   // This only lists the current directory once
     *   val view = FileTreeView.default
     *   view.iterator(Seq(dir / "*.scala", dir / "*.java")).foreach {
     *    case (path: Path, attributes: FileAttributes) =>
     *      println(s"path: $$path, attributes: $$attributes")
     *  }
     *   // This lists the current directory twice
     *   (view.iterator(dir / "*.scala") ++ view.iterator(dir / "*.java")) .foreach {
     *    case (path, attributes) => println(s"path: $$path, attributes: $$attributes")
     *  }
     * }}}
     *
     * @param globs the search queries
     * @return all of the paths that match the search query.
     */
    def iterator(globs: Traversable[Glob]): Iterator[(Path, FileAttributes)] =
      FileTreeView.iterator(globs, fileTreeView)

    /**
     * Returns a filtered of the existing paths on the file system that match the [[Glob]] pattern.
     * It should not throw an IOException and will return an empty sequence if no paths exist that
     * match the patterns. It will print a warning if any of the globs do not have an absolute
     * base path but it will expand the glob to `Paths.get("").toGlob / glob` for traversal. It
     * optimizes traversal so that each directory on the file system is only listed once:
     *
     * {{{
     *   val dir = Paths.get("").toAbsolutePath.toGlob
     *   // This only lists the current directory once
     *   val view = FileTreeView.default
     *   val filter: (Path, FileAttributes) => Boolean = (_, a) => a.isRegularFile
     *   view.iterator(Seq(dir / "*.scala", dir / "*.java"), filter).foreach {
     *    case (path, attributes) => println(s"path: $$path, attributes: $$attributes")
     *  }
     *   // This lists the current directory twice
     *   (view.iterator(dir / "*.scala", filter) ++ view.iterator(dir / "*.java"), filter).foreach {
     *    case (path, attributes) => println(s"path: $$path, attributes: $$attributes")
     *  }
     * }}}
     *
     * @param globs the search queries
     * @param filter the pathfilter
     * @return all of the paths that match the search query.
     */
    def iterator(globs: Traversable[Glob], filter: PathFilter): Iterator[(Path, FileAttributes)] =
      FileTreeView.iterator(globs, fileTreeView, filter)
  }
  private[sbt] type Nio[+T] = FileTreeView[(Path, T)]

  /**
   * Provides a default instance of [[FileTreeView]]. The default may be configured by the
   * `sbt.io.filetreeview` system property. When it is set to `nio`, the built in jvm implementation
   * if used [[FileTreeView.nio]]. Otherwise, [[FileTreeView.native]] will be used.
   * @return the default [[FileTreeView]]
   */
  def default: FileTreeView[(Path, FileAttributes)] =
    if ("nio" == System.getProperty("sbt.io.filetreeview", "")) nio else native
  private[sbt] implicit class NioFileTreeViewOps[T](val view: FileTreeView.Nio[T]) {
    def map[A >: T, B](f: (Path, A) => B): FileTreeView.Nio[B] = {
      val converter: ((Path, A)) => (Path, B) = {
        case (path: Path, attrs) => path -> f(path, attrs)
      }
      path: Path => view.list(path).map(converter)
    }
    def flatMap[B, A >: T](f: (Path, A) => Traversable[B]): FileTreeView.Nio[B] = {
      val converter: ((Path, A)) => Traversable[(Path, B)] = {
        case (path: Path, attrs) => f(path, attrs).map(path -> _)
      }
      path: Path => view.list(path).flatMap(converter(_))
    }
  }

  private[sbt] def all(
      globs: Traversable[Glob],
      view: FileTreeView.Nio[FileAttributes]
  ): Seq[(Path, FileAttributes)] =
    all(globs, view, AllPass)
  private[sbt] def all(
      globs: Traversable[Glob],
      view: FileTreeView.Nio[FileAttributes],
      filter: PathFilter
  ): Seq[(Path, FileAttributes)] =
    iterator(globs, view, filter).toVector

  private[sbt] def iterator(
      globs: Traversable[Glob],
      view: FileTreeView.Nio[FileAttributes]
  ): Iterator[(Path, FileAttributes)] =
    iterator(globs, view, (_, _) => true)
  private[sbt] def iterator(
      globs: Traversable[Glob],
      view: FileTreeView.Nio[FileAttributes],
      filter: PathFilter
  ): Iterator[(Path, FileAttributes)] = {
    /*
     * As an optimization, partition the Globs into Root globs and other kinds. Then for each of
     * the root Globs, we can partition by parent path and aggregate these into a single Glob
     * for each parent. For example, if the input globs are `/foo/bar.txt` and `/foo/baz.txt`, we
     * can aggregate these into the glob: `/foo/{bar.txt,baz.txt}`. In the case where there are
     * 1000 input globs of the form { /foo/a1, /foo/a2, .., /foo/a1000 }, and 10 random files that
     * match that pattern, e.g. /foo/63, the total time to run FileTreeView.list(globs) drops from
     * about 20ms to 1ms.
     */
    val (roots, rest) = globs.toVector.partition { case _: Root => true; case _ => false }
    val rootPaths = roots.map {
      case r: Root => r.root
      case _       => throw new IllegalStateException("Partition failed (should be unreachable).")
    }
    val rootPathsByParent = rootPaths.groupBy(_.getParent)
    val rootPathParams = rootPathsByParent.map {
      case (parent, paths) =>
        paths.map(_.getFileName) match {
          case Seq(fileName) => (parent, 1, Glob(parent, fileName.toString))
          case fileNames     => (parent, 1, Glob(parent, fileNames.mkString("{", ",", "}")))
        }
    }
    val params = (rootPathParams.toSeq ++
      rest.distinct.map(_.fileTreeViewListParameters)).sortBy(_._1)
    var directoryCache: Option[(Path, ConcurrentHashMap[Path, FileAttributes])] = None
    val needListDirectory: Path => Boolean = (path: Path) =>
      params.exists {
        case (base, maxDepth, _) =>
          path.startsWith(base) && base.relativize(path).getNameCount < maxDepth
      }
    val visited = new util.HashSet[Path]
    val pathFilter: Path => Boolean = path => params.exists(_._3.matches(path))
    val totalFilter: (Path, FileAttributes) => Boolean = { (path, attributes) =>
      pathFilter(path) && filter.accept(path, attributes)
    }
    val remainingGlobs = new util.LinkedList[Glob]()
    params.foreach(p => remainingGlobs.add(p._3))
    val remainingPaths = new util.LinkedList[Path]()
    new Iterator[(Path, FileAttributes)] {
      private[this] val buffer = new util.LinkedList[(Path, FileAttributes)]
      private[this] val maybeAdd: ((Path, FileAttributes)) => Unit = {
        case pair @ (path, attributes) =>
          if (totalFilter(path, attributes)) buffer.add(pair)
          ()
      }
      private def listPath(path: Path): Unit = {
        try {
          view.list(path) foreach {
            case pair @ (p, attributes) if attributes.isDirectory =>
              if (needListDirectory(p)) remainingPaths.add(p)
              maybeAdd(pair)
            case pair => maybeAdd(pair)
          }
        } catch {
          case _: NotDirectoryException if !visited.contains(path.getParent) =>
            val map = directoryCache match {
              case Some((parent, m)) if parent == path.getParent => m
              case _ =>
                val map = new ConcurrentHashMap[Path, FileAttributes]()
                try view.list(path.getParent).foreach { case (p, a) => map.put(p, a) } catch {
                  case _: IOException =>
                }
                directoryCache = Some(path.getParent -> map)
                map
            }
            map.get(path) match {
              case null =>
              case a    => maybeAdd(path -> a)
            }
          case _: IOException =>
        }
      }
      @tailrec
      private def fillBuffer(): Unit = {
        remainingPaths.poll match {
          case null =>
            remainingGlobs.poll() match {
              case null =>
              case g =>
                remainingPaths.add(g.base)
                fillBuffer()
            }
          case path if !visited.contains(path) =>
            visited.add(path)
            path.getParent match {
              case null =>
              case p =>
                directoryCache match {
                  case Some((`p`, m)) =>
                    m.get(path) match {
                      case null               =>
                      case a if a.isDirectory => listPath(p)
                      case a                  => maybeAdd(path -> a)
                    }
                  case _ => listPath(path)
                }
            }
            if (buffer.isEmpty) fillBuffer()
          // if we've already visited the path, go to next
          case _ =>
            if (buffer.isEmpty) fillBuffer()
        }
      }
      override def hasNext: Boolean = !buffer.isEmpty
      override def next(): (Path, FileAttributes) = {
        val res = buffer.poll()
        if (buffer.isEmpty) {
          fillBuffer()
        }
        res
      }
      fillBuffer()
    }
  }
}
