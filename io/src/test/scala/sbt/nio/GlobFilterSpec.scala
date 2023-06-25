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

package sbt.nio

import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import sbt.io.IO
import sbt.io.syntax._
import sbt.nio.file.{ AnyPath, Glob, RecursiveGlob }
import sbt.nio.file.Glob.GlobOps

class GlobFilterSpec extends AnyFlatSpec {
  "GlobAsFilter" should "work with simple files" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val filter = Glob(dir).toFileFilter
    assert(filter.accept(dir))
    assert(!filter.accept(file))
    assert(!filter.accept(nestedFile))
  }
  it should "work with globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = Glob(dir, AnyPath)
    assert(!glob.matches(dir.toPath))
    assert(glob.matches(file.toPath))
    assert(!glob.matches(nestedFile.toPath))
  }
  it should "work with recursive globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = Glob(dir, RecursiveGlob)
    assert(!glob.matches(dir.toPath))
    assert(glob.matches(file.toPath))
    assert(glob.matches(nestedFile.toPath))
  }
  it should "work with depth" in {
    val base = Paths.get("").toAbsolutePath.getRoot.resolve("foo").resolve("bar")
    assert(Glob(base, s"*/*.txt").matches(base.resolve("foo").resolve("bar.txt")))
    assert(!Glob(base, s"*/*/*.txt").matches(base.resolve("foo").resolve("bar.txt")))
    assert(Glob(base, s"*/*/*.txt").matches(base.resolve("foo").resolve("baz").resolve("bar.txt")))
    assert(!Glob(base, s"*/*/*.txt").matches(base.resolve("foo").resolve("baz").resolve("bar.tx")))
    assert(Glob(base, s"*/**/*.txt").matches(base.resolve("foo").resolve("bar.txt")))
    assert(
      Glob(base, s"*/**/*.txt")
        .matches(base.resolve("foo").resolve("bar").resolve("baz").resolve("bar.txt"))
    )
    assert(
      !Glob(base, s"*/**/*.txt")
        .matches(base.resolve("foo").resolve("bar").resolve("baz").resolve("bar.tx"))
    )
  }
}
