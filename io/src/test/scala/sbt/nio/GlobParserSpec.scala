package sbt.nio

import java.nio.file.Paths

import org.scalatest.FlatSpec
import TestHelpers._

class GlobParserSpec extends FlatSpec {
  it should "parse pure paths" in {
    val absolute = Glob.parse(p"$basePath/baz")
    assert(absolute == Glob(Paths.get(p"$basePath/baz"), (0, 0), AllPass))
    val relative = Glob.parse("bar/baz")
    assert(relative == Glob(Paths.get("bar/baz"), (0, 0), AllPass))
  }
  it should "parse paths with range" in {
    val children = Glob.parse(p"$basePath/*")
    assert(children == Glob(basePath, (1, 1), AllPass))
    val subChildren = Glob.parse(p"$basePath/*/*")
    assert(subChildren == Glob(basePath, (2, 2), AllPass))
    val recursive = Glob.parse(p"$basePath/**")
    assert(recursive == Glob(basePath, (1, Int.MaxValue), AllPass))
    val recursiveSubchildren = Glob.parse(p"$basePath/*/*/**")
    assert(recursiveSubchildren == Glob(basePath, (3, Int.MaxValue), AllPass))
  }
  it should "parse paths with filters" in {
    val exact = Glob.parse(p"$basePath/*/foo.txt")
    assert(exact == Glob(Paths.get(p"$basePath"), (2, 2), new ExactNameFilter("foo.txt")))
    val extension = Glob.parse(p"$basePath/**/*.s")
    assert(extension == Glob(Paths.get(p"$basePath"), (1, Int.MaxValue), new ExtensionFilter("s")))
    val prefix = Glob.parse(p"foo/bar/*/*/foo*")
    assert(prefix == Glob(Paths.get(p"foo/bar"), (3, 3), new SplitFilter("foo", "")))
    val suffix = Glob.parse(p"foo/bar/*/*/*bar")
    assert(suffix == Glob(Paths.get(p"foo/bar"), (3, 3), new SplitFilter("", "bar")))
    val prefixAndSuffix = Glob.parse(p"$basePath/*/**/foo*bar")
    assert(prefixAndSuffix == Glob(basePath, (2, Int.MaxValue), new SplitFilter("foo", "bar")))
  }
}
