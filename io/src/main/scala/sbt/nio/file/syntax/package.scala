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

import java.io.File
import java.nio.file.Path

import sbt.nio.file.Glob.{ FileOps, PathOps }

package object syntax extends syntax0
private[sbt] trait syntax0 {
  implicit def pathToPathOps(path: Path): PathOps = new PathOps(path)
  implicit def fileToFileOps(file: File): FileOps = new FileOps(file)
}
