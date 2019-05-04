/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.nio

import java.io.IOException
import java.nio.file._

import com.swoval.files.FileTreeViews
import sbt.nio.file.{ FileAttributes, FileTreeView }

import scala.collection.JavaConverters._

private[sbt] object DefaultFileTreeView extends FileTreeView.Nio[FileAttributes] {
  private[this] val fileTreeView =
    if ("nio" == sys.props.getOrElse("sbt.pathfinder", ""))
      FileTreeViews.getNio(true)
    else
      FileTreeViews.getDefault(true)
  override def list(path: Path): Seq[(Path, FileAttributes)] = {
    try {
      fileTreeView
        .list(path, 0, _ => true)
        .iterator
        .asScala
        .map { typedPath =>
          typedPath.getPath ->
            FileAttributes(
              isDirectory = typedPath.isDirectory,
              isOther = false,
              isRegularFile = typedPath.isFile,
              isSymbolicLink = typedPath.isSymbolicLink
            )
        }
        .toVector
    } catch {
      case _: IOException => Vector.empty
    }
  }
}
