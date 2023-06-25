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

package sbt.io

import java.nio.file.Files

import org.scalatest.flatspec.AnyFlatSpec
import sbt.io.syntax._

class PathFinderCombinatorSpec extends AnyFlatSpec {
  "PathFinderCombinator" should "provide extension methods for File" in IO.withTemporaryDirectory {
    dir =>
      val file = Files.createFile(dir.toPath.resolve("file")).toFile
      assert((dir +++ file).get() == Seq(dir, file))
      assert(((dir: PathFinder.Combinator) +++ file).get() == Seq(dir, file))
      assert(((dir: PathFinder) +++ file).get() == Seq(dir, file))
  }
}
