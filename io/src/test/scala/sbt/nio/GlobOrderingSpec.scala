/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.nio

import java.io.File

import org.scalatest.FlatSpec
import sbt.io.IO
import sbt.nio.file.{ Glob, RecursiveGlob }

class GlobOrderingSpec extends FlatSpec {
  "Globs" should "be ordered" in IO.withTemporaryDirectory { dir =>
    val subdir = new File(dir, "subdir")
    assert(Seq(Glob(subdir), Glob(dir)).sorted == Seq(Glob(dir), Glob(subdir)))
  }
  they should "fall back on depth" in IO.withTemporaryDirectory { dir =>
    val recursive = Glob(dir, RecursiveGlob)
    val nonRecursive = Glob(dir)
    assert(Seq(nonRecursive, recursive).sorted == Seq(recursive, nonRecursive))
  }
  they should "not stack overflow" in IO.withTemporaryDirectory { dir =>
    val exact = Glob(dir.toPath.resolve("foo"))
    val fullFile = sbt.internal.nio.Globs(dir.toPath, true, sbt.io.HiddenFileFilter)
    assert(Seq(exact, fullFile).sorted == Seq(exact, fullFile))
  }
}
