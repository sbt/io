package sbt.nio

import org.scalatest.FlatSpec
import sbt.nio.TestHelpers._
import sbt.nio.file.{ AnyPath, Glob, RecursiveGlob, RelativeGlob }

class GlobParserSpec extends FlatSpec {
  it should "parse pure paths" in {
    assert(Glob(p"$basePath/baz") == Glob(basePath.resolve("baz")))
    val absolute = Glob(p"$basePath/baz")
    assert(absolute == Glob(basePath.resolve("baz")))
    val relative = Glob(p"bar/baz")
    assert(relative == RelativeGlob("bar", "baz"))
  }
  it should "parse paths with range" in {
    val children = Glob(p"$basePath/*")
    assert(children == Glob(basePath, AnyPath))
    val subChildren = Glob(p"$basePath/*/*")
    assert(subChildren == Glob(basePath, AnyPath / AnyPath))
    val recursive = Glob(p"$basePath/**")
    assert(recursive == Glob(basePath, RecursiveGlob))
    val recursiveSubchildren = Glob(p"$basePath/*/*/**")
    assert(recursiveSubchildren == Glob(basePath, AnyPath / AnyPath / RecursiveGlob))
  }
  it should "parse paths with filters" in {
    val exact = Glob(p"$basePath/*/foo.txt")
    assert(exact == Glob(basePath, AnyPath / "foo.txt"))
    val extension = Glob(p"$basePath/**/*.s")
    assert(extension == Glob(basePath, RecursiveGlob / "*.s"))
    val prefix = Glob(p"foo/bar/*/*/foo*")
    assert(prefix == RelativeGlob("foo", "bar") / AnyPath / AnyPath / "foo*")
    val suffix = Glob(p"foo/bar/*/*/*bar")
    assert(suffix == RelativeGlob("foo", "bar") / AnyPath / AnyPath / "*bar")
    val prefixAndSuffix = Glob(p"$basePath/*/**/foo*bar")
    assert(prefixAndSuffix == Glob(basePath) / AnyPath / RecursiveGlob / "foo*bar")
  }
}
