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
import java.nio.file.{ Files, NotDirectoryException, Path }
import java.util
import java.util.concurrent.ConcurrentHashMap

import sbt.internal.nio.SwovalFileTreeView

import scala.annotation.tailrec
import scala.collection.JavaConverters._

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
  val native: FileTreeView.Nio[FileAttributes] = SwovalFileTreeView
  val nio: FileTreeView.Nio[FileAttributes] = (path: Path) => {
    val paths = Files.list(path).iterator.asScala
    paths.flatMap(p => FileAttributes(p).toOption.map(p -> _)).toIndexedSeq
  }

  /**
   * Adds additional methods to [[FileTreeView]]. This api may be changed so it should not be
   * imported directly.
   * @param fileTreeView the [[FileTreeView]] to augment.
   */
  implicit class Ops(val fileTreeView: FileTreeView.Nio[FileAttributes]) extends AnyVal {
    def list(glob: Glob): Seq[(Path, FileAttributes)] = all(glob :: Nil, fileTreeView)
    def list(globs: Traversable[Glob]): Seq[(Path, FileAttributes)] = all(globs, fileTreeView)
    def iterator(glob: Glob): Iterator[(Path, FileAttributes)] =
      FileTreeView.iterator(glob :: Nil, fileTreeView)
    def iterator(globs: Traversable[Glob]): Iterator[(Path, FileAttributes)] =
      FileTreeView.iterator(globs, fileTreeView)
  }
  private[sbt] type Nio[+T] = FileTreeView[(Path, T)]
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
    all(globs, view, (_, _) => true)
  private[sbt] def all(
      globs: Traversable[Glob],
      view: FileTreeView.Nio[FileAttributes],
      filter: (Path, FileAttributes) => Boolean
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
      filter: (Path, FileAttributes) => Boolean
  ): Iterator[(Path, FileAttributes)] = {
    val params = globs.toSeq.sorted.distinct.map(_.fileTreeViewListParameters)
    var directoryCache: Option[(Path, ConcurrentHashMap[Path, FileAttributes])] = None
    val needListDirectory: Path => Boolean = (path: Path) =>
      params.exists {
        case (base, maxDepth, _) =>
          path.startsWith(base) && base.relativize(path).getNameCount < maxDepth
      }
    val visited = new util.HashSet[Path]
    val pathFilter: Path => Boolean = path => params.exists(_._3.matches(path))
    val totalFilter: (Path, FileAttributes) => Boolean = { (path, attributes) =>
      pathFilter(path) && filter(path, attributes)
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
