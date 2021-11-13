/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
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
