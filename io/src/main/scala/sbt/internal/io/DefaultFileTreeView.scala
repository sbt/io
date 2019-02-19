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

import java.nio.file.{ NoSuchFileException, NotDirectoryException, Path => NioPath }

import com.swoval.files.{ FileTreeViews, TypedPath }
import com.swoval.functional.Filter
import sbt.io._

import scala.collection.JavaConverters._

private[sbt] object DefaultFileTreeView extends NioFileTreeView[SimpleFileAttributes] {
  private[this] val fileTreeView =
    if ("nio" == sys.props.getOrElse("sbt.pathfinder", ""))
      FileTreeViews.getNio(true)
    else
      FileTreeViews.getDefault(true)

  override def list(glob: Glob, filter: ((NioPath, SimpleFileAttributes)) => Boolean)
    : Seq[(NioPath, SimpleFileAttributes)] = {
    try {
      fileTreeView
        .list(glob.base, glob.depth, new Filter[TypedPath] {
          override def accept(t: TypedPath): Boolean = true
        })
        .asScala
        .flatMap { typedPath =>
          val path = typedPath.getPath
          val attributes = SimpleFileAttributes.get(typedPath.exists,
                                                    typedPath.isDirectory,
                                                    typedPath.isFile,
                                                    typedPath.isSymbolicLink)
          val pair = path -> attributes
          if (glob.filter(path) && filter(pair)) Some(pair) else None
        }
        .toIndexedSeq
    } catch {
      case _: NoSuchFileException | _: NotDirectoryException =>
        Nil
    }
  }
}
