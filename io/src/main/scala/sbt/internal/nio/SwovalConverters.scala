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

import java.nio.file.Path

import com.swoval.files.FileTreeViews
import com.swoval.functional.{ Either => SEither }
import sbt.nio.file.{ FileAttributes, FileTreeView }

import scala.collection.immutable.VectorBuilder

/**
 * Utilities for converting between swoval and sbt data types.
 */
private[nio] object SwovalConverters {
  implicit class RangeOps(val range: (Int, Int)) extends AnyVal {
    def toSwovalDepth: Int = range._2 match {
      case Int.MaxValue => Int.MaxValue
      case d            => d - 1
    }
  }
  implicit class SwovalEitherOps[L, R](val either: SEither[L, R]) extends AnyVal {
    def asScala[R0](implicit f: R => R0): Either[L, R0] = either match {
      case l: com.swoval.functional.Either.Left[L, R] =>
        Left(com.swoval.functional.Either.leftProjection(l).getValue)
      case r: com.swoval.functional.Either.Right[L, R] => Right(f(r.get()))
    }
  }
}
private[sbt] object SwovalFileTreeView extends FileTreeView.Nio[FileAttributes] {
  private[this] val view = FileTreeViews.getDefault(true)
  override def list(path: Path): Seq[(Path, FileAttributes)] = {
    val result = new VectorBuilder[(Path, FileAttributes)]
    view.list(path, 0, _ => true).forEach { typedPath =>
      result += typedPath.getPath ->
        FileAttributes(
          isDirectory = typedPath.isDirectory,
          isOther = false,
          isRegularFile = typedPath.isFile,
          isSymbolicLink = typedPath.isSymbolicLink
        )
    }
    result.result()
  }
}
