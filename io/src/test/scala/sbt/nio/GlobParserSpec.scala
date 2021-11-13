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

import org.scalatest.flatspec.AnyFlatSpec
import sbt.nio.TestHelpers._
import sbt.nio.file.{ AnyPath, Glob, RecursiveGlob, RelativeGlob }

class GlobParserSpec extends AnyFlatSpec {
  it should "parse pure paths" in {
    assert(Glob(s"$basePath/baz") == Glob(basePath.resolve("baz")))
    val absolute = Glob(s"$basePath/baz")
    assert(absolute == Glob(basePath.resolve("baz")))
    val relative = Glob(s"bar/baz")
    assert(relative == RelativeGlob("bar", "baz"))
  }
  it should "parse paths with range" in {
    val children = Glob(s"$basePath/*")
    assert(children == Glob(basePath, AnyPath))
    val subChildren = Glob(s"$basePath/*/*")
    assert(subChildren == Glob(basePath, AnyPath / AnyPath))
    val recursive = Glob(s"$basePath/**")
    assert(recursive == Glob(basePath, RecursiveGlob))
    val recursiveSubchildren = Glob(s"$basePath/*/*/**")
    assert(recursiveSubchildren == Glob(basePath, AnyPath / AnyPath / RecursiveGlob))
  }
  it should "parse paths with filters" in {
    val exact = Glob(s"$basePath/*/foo.txt")
    assert(exact == Glob(basePath, AnyPath / "foo.txt"))
    val extension = Glob(s"$basePath/**/*.s")
    assert(extension == Glob(basePath, RecursiveGlob / "*.s"))
    val prefix = Glob(s"foo/bar/*/*/foo*")
    assert(prefix == RelativeGlob("foo", "bar") / AnyPath / AnyPath / "foo*")
    val suffix = Glob(s"foo/bar/*/*/*bar")
    assert(suffix == RelativeGlob("foo", "bar") / AnyPath / AnyPath / "*bar")
    val prefixAndSuffix = Glob(s"$basePath/*/**/foo*bar")
    assert(prefixAndSuffix == Glob(basePath) / AnyPath / RecursiveGlob / "foo*bar")
  }
}
