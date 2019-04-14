package sbt.nio

import java.nio.file.Paths

import org.scalatest.FlatSpec

import scala.util.Try

class GlobParserSpec extends FlatSpec {
  it should "parse pure paths" in {
    val absolute = Glob.parse("/foo/bar/baz")
    assert(absolute == Glob(Paths.get("/foo/bar/baz"), (0, 0), AllPass))
    val relative = Glob.parse("bar/baz")
    assert(relative == Glob(Paths.get("bar/baz"), (0, 0), AllPass))
  }
  it should "parse pure paths with spaces" in {
    val absolute = Glob.parse("/ foo    /   bar / baz")
    assert(absolute == Glob(Paths.get("/foo/bar/baz"), (0, 0), AllPass))
    val relative = Glob.parse("bar /     baz")
    assert(relative == Glob(Paths.get("bar/baz"), (0, 0), AllPass))
  }
  it should "parse paths with range" in {
    val children = Glob.parse("/foo/bar/*")
    assert(children == Glob(Paths.get("/foo/bar"), (1, 1), AllPass))
    val subChildren = Glob.parse("/foo/bar/*/*")
    assert(subChildren == Glob(Paths.get("/foo/bar"), (2, 2), AllPass))
    val recursive = Glob.parse("/foo/bar/**")
    assert(recursive == Glob(Paths.get("/foo/bar"), (1, Int.MaxValue), AllPass))
    // throw some random spaces in just to ensure spaces work with '*' parts
    val recursiveSubchildren = Glob.parse("/foo /bar/ * / */ **  ")
    assert(recursiveSubchildren == Glob(Paths.get("/foo/bar"), (3, Int.MaxValue), AllPass))
  }
  it should "throw an exception for glob parameters after a recursive parameter" in {
    assert(Try(Glob.parse("/foo/bar/*/**/*")).isFailure)
  }
  it should "parse paths with no base path" in {
    val relative = Glob.parse("**")
    assert(relative == Glob(Paths.get(""), (0, Int.MaxValue), AllPass))
  }
  it should "parse paths with filters" in {
    val exact = Glob.parse("/foo/bar/*/foo.txt")
    assert(exact == Glob(Paths.get("/foo/bar"), (2, 2), new ExactNameFilter("foo.txt")))
    val extension = Glob.parse("/foo/bar / ** / *.s")
    assert(extension == Glob(Paths.get("/foo/bar"), (1, Int.MaxValue), new ExtensionFilter("s")))
    val prefix = Glob.parse("foo/bar/ * / */foo*")
    assert(prefix == Glob(Paths.get("foo/bar"), (3, 3), new SplitFilter("foo", "")))
    val suffix = Glob.parse("foo/bar/ * / */*bar")
    assert(suffix == Glob(Paths.get("foo/bar"), (3, 3), new SplitFilter("", "bar")))
    val prefixAndSuffix = Glob.parse("/foo/bar/ * / ** / foo*bar")
    assert(
      prefixAndSuffix == Glob(Paths.get("/foo/bar"),
                              (2, Int.MaxValue),
                              new SplitFilter("foo", "bar")))
  }
}
