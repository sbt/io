/*
 * sbt IO
 * Copyright Scala Center, Lightbend, and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * SPDX-License-Identifier: Apache-2.0
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
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
