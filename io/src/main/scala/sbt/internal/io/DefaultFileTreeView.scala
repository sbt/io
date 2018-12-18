package sbt.internal.io

import java.nio.file.{ NoSuchFileException, NotDirectoryException, Path }

import com.swoval.files.{ FileTreeViews, TypedPath => STypedPath }
import com.swoval.functional.Filters
import sbt.internal.io.SwovalConverters._
import sbt.io.{ FileTreeView, TypedPath }

import scala.collection.JavaConverters._

private[sbt] object DefaultFileTreeView extends FileTreeView {
  private[this] val fileTreeView =
    if ("nio" == sys.props.getOrElse("sbt.pathfinder", ""))
      FileTreeViews.getNio(true)
    else
      FileTreeViews.getDefault(true)

  /**
   * List the contents of the current directory
   *
   * @param path      the path to list
   * @param maxDepth  controls the depth of children of the path to include in the results. When
   *                  maxDepth is -1, [[list]] should only return the TypedPath for this directory.
   *                  For non-negative values, [[list]] should return only entries whose relativized
   *                  path has {{{maxDepth - 1}}} elements. For example, when maxDepth is zero, all of
   *                  the children of the path should be included in the result, but none of the
   *                  children of any of the subdirectories should be incldued.
   *
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
      case _: NoSuchFileException | _: NotDirectoryException =>
        Nil
    }
  }

  override def close(): Unit = ()
}
