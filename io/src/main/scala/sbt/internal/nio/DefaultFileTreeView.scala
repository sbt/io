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

import java.nio.file.{ NoSuchFileException, NotDirectoryException, Path => NioPath }

import com.swoval.files.{ FileTreeViews, TypedPath }
import sbt.internal.io.Retry
import sbt.internal.nio.SwovalConverters._
import sbt.nio.{ FileAttributes, FileTreeView, Glob }

import scala.collection.JavaConverters._

private[sbt] object DefaultFileTreeView extends FileTreeView.Nio[FileAttributes] {
  private[this] val fileTreeView =
    if ("nio" == sys.props.getOrElse("sbt.pathfinder", ""))
      FileTreeViews.getNio(true)
    else
      FileTreeViews.getDefault(true)

  override def list(glob: Glob): Seq[(NioPath, FileAttributes)] =
    Retry {
      try {
        val collector: TypedPath => Seq[(NioPath, FileAttributes)] = typedPath => {
          val path = typedPath.getPath
          val attributes = FileAttributes(isDirectory = typedPath.isDirectory,
                                          isOther = false,
                                          isRegularFile = typedPath.isFile,
                                          isSymbolicLink = typedPath.isSymbolicLink)
          val pair = path -> attributes
          if (glob.filter.accept(path)) pair :: Nil else Nil
        }
        (if (glob.range._1 == 0) {
           fileTreeView
             .list(glob.base, -1, (_: TypedPath) => true)
             .asScala
             .flatMap(collector)
             .toIndexedSeq
         } else Vector.empty[(NioPath, FileAttributes)]) ++ (if (glob.range._2 > 0)
                                                               fileTreeView
                                                                 .list(glob.base,
                                                                       glob.range.toSwovalDepth,
                                                                       (_: TypedPath) => true)
                                                                 .asScala
                                                                 .flatMap(collector)
                                                                 .toIndexedSeq
                                                             else
                                                               Vector
                                                                 .empty[(NioPath, FileAttributes)])
      } catch {
        case _: NoSuchFileException | _: NotDirectoryException =>
          Nil
      }
    }
}
