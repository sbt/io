package sbt.internal.io

import java.nio.file.{ NoSuchFileException, NotDirectoryException, Path }

import com.swoval.files.{ TypedPath => STypedPath }
import com.swoval.files.FileTreeViews
import com.swoval.functional.Filters
import sbt.io.{ FileTreeView, TypedPath }

import scala.collection.JavaConverters._
import SwovalConverters._

import scala.util.Try

private[sbt] object DefaultFileTreeView extends FileTreeView {
  private[this] val fileTreeView =
    if ("nio" == System.getProperty("sbt.pathfinder.implementation"))
      FileTreeViews.getNio(true)
    else
      FileTreeViews.getDefault(true)

  /**
   * List the contents of the current directory
   *
   * @param path      the path to list
   * @param recursive toggles whether or not to recursively list the directory contents
   * @param filter    only return files accepted by the filter
   * @return a sequence of [[TypedPath]].
   */
  override def list(path: Path, maxDepth: Int, filter: TypedPath => Boolean): Seq[TypedPath] = {
    try {
      fileTreeView
        .list(path, maxDepth, Filters.AllPass)
        .asScala
        .toSeq
        .flatMap((_: STypedPath).asSbt match {
          case tp if filter(tp) => Some(tp)
          case _                => None
        })
    } catch {
      case _: NotDirectoryException | _: NoSuchFileException =>
        Try(Seq(TypedPath(path))).getOrElse(Nil)
    }
  }

  override def close(): Unit = ()
}
