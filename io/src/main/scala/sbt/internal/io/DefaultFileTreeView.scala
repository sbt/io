/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.io

import java.nio.file.{ NoSuchFileException, NotDirectoryException }

import com.swoval.files.{ FileTreeViews, TypedPath => STypedPath }
import com.swoval.functional.Filter
import sbt.internal.io.SwovalConverters._
import sbt.io.{ FileTreeView, Glob, TypedPath }

import scala.collection.JavaConverters._

private[sbt] object DefaultFileTreeView extends FileTreeView {
  private[this] val fileTreeView =
    if ("nio" == sys.props.getOrElse("sbt.pathfinder", ""))
      FileTreeViews.getNio(true)
    else
      FileTreeViews.getDefault(true)

  override def list(glob: Glob): Seq[TypedPath] = {
    try {
      val globFilter = glob.toTypedPathFilter
      fileTreeView
        .list(glob.base, glob.depth, new Filter[STypedPath] {
          override def accept(t: STypedPath): Boolean = true
        })
        .asScala
        .flatMap((_: STypedPath).asSbt match {
          case tp if globFilter(tp) => Some(tp)
          case _                    => None
        })
        .toIndexedSeq
    } catch {
      case _: NoSuchFileException | _: NotDirectoryException =>
        Nil
    }
  }

  override def close(): Unit = ()
}
