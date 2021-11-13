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
import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import sbt.nio.TestHelpers._
import sbt.nio.file.Glob.GlobOps
import sbt.nio.file.Glob.RelativeGlobViewOption
import sbt.nio.file.RelativeGlob._
import sbt.nio.file.syntax._
import sbt.nio.file.{ AnyPath, Glob, RecursiveGlob, RelativeGlob }

import scala.util.Try

class GlobSyntaxSpec extends AnyFlatSpec {
  "path builders" should "use `*` and `**` objects" in {
    assert(Glob(basePath, AnyPath).matches(basePath.resolve("foo")))
    assert(!Glob(basePath, AnyPath).matches(basePath.resolve("foo").resolve("bar")))
    assert(!Glob(basePath, AnyPath).matches(basePath.getParent))
    assert(Glob(basePath, AnyPath) == Glob(basePath, AnyPath))
    assert(Glob(basePath, RecursiveGlob) == Glob(basePath, RecursiveGlob))
    assert(Glob(basePath, AnyPath / RecursiveGlob) == Glob(basePath, AnyPath / RecursiveGlob))
    assert(Glob(basePath, s"*/**") == Glob(basePath, AnyPath / RecursiveGlob))
    assert(!Glob(basePath, AnyPath).matches(basePath))
    assert(!Glob(basePath, RecursiveGlob).matches(basePath))
  }
  they should "apply question marks" in {
    assert(Glob(basePath, "?foo").matches(basePath.resolve("afoo")))
    assert(!Glob(basePath, "?foo").matches(basePath.resolve("afoob")))
    assert(Glob(basePath, "?foo?").matches(basePath.resolve("afoob")))
    assert(!Glob(basePath, "?foo?").matches(basePath.resolve("afoo")))
    assert(!Glob(basePath, "?foo?").matches(basePath.resolve("afoobc")))
    assert(Glob(basePath, "?foo*").matches(basePath.resolve("afoobc")))
    assert(Glob(basePath, "foo?").matches(basePath.resolve("fooa")))
    assert(Glob(basePath, "foo?a").matches(basePath.resolve("fooaa")))
  }
  they should "apply ranges" in {
    assert(Glob(basePath, "foo[a-d]b").matches(basePath.resolve("fooab")))
    assert(!Glob(basePath, "foo[a-d]b").matches(basePath.resolve("fooeb")))
    assert(Glob(basePath, "*foo[a-d]b").matches(basePath.resolve("fooab")))
    assert(Glob(basePath, "*foo[a-d]b").matches(basePath.resolve("abcdefooab")))
    assert(!Glob(basePath, "foo[a-d]b").matches(basePath.resolve("abcdefooeb")))
    assert(Glob(basePath, s"**/*foo[a-d]b").matches(basePath.resolve("abcdefooab")))
    assert(
      Glob(basePath, s"**/*/*foo[a-d]b").matches(basePath.resolve("bar").resolve("abcdefooab"))
    )
    assert(
      Glob(basePath, s"**/*/*foo[a-d]b")
        .matches(basePath.resolve("bar").resolve("baz").resolve("buzz").resolve("abcdefooab"))
    )
  }
  they should "apply extension filters" in {
    assert(Glob(basePath, "*.txt").matches(basePath.resolve("foo.txt")))
    assert(!Glob(basePath, "*.txt").matches(basePath.resolve("foo.txt1")))
    assert(!Glob(basePath, "*.txt").matches(basePath.resolve("bar").resolve("foo.txt")))
    assert(Glob(basePath, "*.{txt,md}").matches(basePath.resolve("foo.txt")))
    assert(Glob(basePath, "*.{txt,md}").matches(basePath.resolve("foo.md")))
    assert(!Glob(basePath, "*.{txt,md}").matches(basePath.resolve("foo.scala")))
    val complexGlob = Glob(basePath, AnyPath / RecursiveGlob / AnyPath / "*.{txt,md}")
    assert(
      complexGlob.matches(basePath.resolve("foo").resolve("bar").resolve("buzz").resolve("foo.txt"))
    )
    assert(complexGlob.matches(basePath.resolve("foo").resolve("bar").resolve("foo.txt")))
    assert(!complexGlob.matches(basePath.resolve("bar").resolve("foo.txt")))
  }
  they should "apply prefix filters" in {
    assert(Glob(basePath, "foo*").matches(basePath.resolve("foo")))
    assert(Glob(basePath, "foo*").matches(basePath.resolve("foo.txt")))
    assert(Glob(basePath, "foo*").matches(basePath.resolve("foo.txt1")))
    assert(!Glob(basePath, "foo*").matches(basePath.resolve("bar").resolve("foo.txt")))
    val complexGlob = Glob(basePath, AnyPath / RecursiveGlob / AnyPath / "foo*")
    assert(
      complexGlob.matches(basePath.resolve("foo").resolve("bar").resolve("buzz").resolve("foo.txt"))
    )
    assert(complexGlob.matches(basePath.resolve("foo").resolve("bar").resolve("foo.txt")))
    assert(!complexGlob.matches(basePath.resolve("bar").resolve("foo.txt")))
  }
  they should "apply suffix filters" in {
    assert(Glob(basePath, "*foo").matches(basePath.resolve("foo")))
    assert(Glob(basePath, "*foo").matches(basePath.resolve("barfoo")))
    assert(!Glob(basePath, "*foo").matches(basePath.resolve("bar").resolve("abcdfoo")))
    val complexGlob = Glob(basePath, AnyPath / RecursiveGlob / AnyPath / "*foo")
    assert(
      complexGlob.matches(basePath.resolve("foo").resolve("bar").resolve("buzz").resolve("abcdfoo"))
    )
    assert(complexGlob.matches(basePath.resolve("foo").resolve("bar").resolve("abcdfoo")))
    assert(!complexGlob.matches(basePath.resolve("bar").resolve("abcdfoo")))
  }
  they should "apply split filters" in {
    assert(Glob(basePath, "foo*bar").matches(basePath.resolve("foobar")))
    assert(Glob(basePath, "foo*bar").matches(basePath.resolve("fooabcbar")))
    assert(Glob(basePath.getParent, s"bar/foo*bar").matches(basePath.resolve("fooabcbar")))
    assert(Glob(basePath, RecursiveGlob / "foo*bar").matches(basePath.resolve("fooabcbar")))
    assert(
      Glob(basePath, RecursiveGlob / "foo*bar")
        .matches(basePath.resolve("baz").resolve("buzz").resolve("fooabcbar"))
    )
  }
  they should "convert strings" in {
    assert((s"$basePath/*": Glob) == Glob(basePath, AnyPath))
    assert((s"$basePath/**": Glob) == Glob(basePath, RecursiveGlob))
    assert((s"$basePath/*/*.txt": Glob) == Glob(basePath, AnyPath / "*.txt"))
    assert((s"$basePath/**/*.txt": Glob) == Glob(basePath, RecursiveGlob / "*.txt"))
    assert((s"$basePath/*/*/*.txt": Glob) == Glob(basePath, AnyPath / AnyPath / "*.txt"))
    assert((s"$basePath/*/**/*.txt": Glob) == Glob(basePath, AnyPath / RecursiveGlob / "*.txt"))
    assert(Glob(basePath, "foo/bar/*.txt") == Glob(basePath, RelativeGlob("foo") / "bar" / "*.txt"))
  }
  they should "handle escaped characters" in {
    assert((basePath.toString + File.separator + "\\{": Glob).matches(basePath.resolve("{")))
    assert((basePath.toString + File.separator + "\\(": Glob).matches(basePath.resolve("(")))
  }
  "base" should "warn on relative paths" in {
    {
      implicit val option: RelativeGlobViewOption = RelativeGlobViewOption.Error
      assert(Try(Glob(Paths.get("ivy"), AnyPath).base).isFailure)
    }
    {
      implicit val option: RelativeGlobViewOption = RelativeGlobViewOption.Ignore
      assert(Glob(Paths.get("ivy"), AnyPath).base == Paths.get("ivy").toAbsolutePath)
    }
  }
  it should "work with relative globs with file name prefix" in {
    assert(Glob(basePath, RelativeGlob("baz") / "buzz" / AnyPath).base == basePath / "baz" / "buzz")
    assert((basePath.toGlob / "baz" / "buzz" / AnyPath).base == basePath / "baz" / "buzz")
  }
  "show" should "represent globs like the shell" in {
    val sep = File.separatorChar
    assert(Glob(basePath, "foo.txt").toString == s"$basePath${sep}foo.txt")
    assert(Glob(basePath, "*").toString == s"$basePath${sep}*")
    assert((s"$basePath/*": Glob).toString == s"$basePath${sep}*")
    // extensions
    assert(Glob(basePath, "*.txt").toString == s"$basePath${sep}*.txt")

    assert(Glob(basePath, "*.{txt,md}").toString == s"$basePath${sep}*.{txt,md}")
    assert(Glob(basePath.getParent, RelativeGlob("bar") / "baz").toString == s"$basePath${sep}baz")
  }
  "syntax" should "work" in {
    assert(basePath / "foo" == basePath.resolve("foo"))
    assert(basePath.toGlob / AnyPath == Glob(basePath, AnyPath))
    assert(
      basePath.toGlob / RecursiveGlob / AnyPath / "*.txt" == Glob(
        basePath,
        RecursiveGlob / AnyPath / "*.txt"
      )
    )
    assert(basePath.toGlob / * == Glob(basePath, AnyPath))
    assert(basePath.toGlob / ** / * / "*.txt" == Glob(basePath, RecursiveGlob / AnyPath / "*.txt"))
  }
  "file tree view params" should "work with relative paths" in {
    implicit val option: RelativeGlobViewOption = RelativeGlobViewOption.Ignore
    assert(Glob(s"./foo").fileTreeViewListParameters._3.matches(Paths.get("foo").toAbsolutePath))
    assert(
      Glob(s"./foo/*").fileTreeViewListParameters._3.matches(Paths.get(s"foo/bar").toAbsolutePath)
    )
  }
  "regex syntax" should "apply patterns" in {
    assert(Glob(basePath, ".*.txt".r).matches(basePath.resolve("foo.txt")))
    assert(!Glob(basePath, ".*.txt".r).matches(basePath.resolve("foo.tx")))
    assert(!Glob(basePath, "foo/.*.txt".r).matches(basePath.resolve("foo.txt")))
    assert(Glob(basePath, "foo/.*.txt".r).matches(basePath.resolve("foo").resolve("foo.txt")))
    assert(Glob(basePath, "foo/.*.txt".r).matches(basePath.resolve("foo").resolve("foo.txt")))
    assert((Glob(basePath) / ".*.txt".r).matches(basePath.resolve("foo.txt")))
    assert(!(Glob(basePath) / ".*.txt".r).matches(basePath.resolve("foo.tx")))
    assert((Glob(basePath) / ** / ".*.txt".r).matches(basePath.resolve("foo.txt")))
    assert(!(Glob(basePath) / ** / ".*.txt".r).matches(basePath.resolve("foo.tx")))
    assert((Glob(basePath) / ** / ".*.txt".r).matches(basePath.resolve("bar").resolve("foo.txt")))
    assert(!(Glob(basePath) / ** / ".*.txt".r).matches(basePath.resolve("bar").resolve("foo.tx")))
    assert((Glob(basePath) / ** / ".*.txt$".r).matches(basePath.resolve("bar").resolve("foo.txt")))
    assert(
      !(Glob(basePath) / ** / ".*.txt$".r).matches(basePath.resolve("bar").resolve("foo.txt4"))
    )
  }
  "dot files" should "not be excluded by default" in {
    assert(Glob(basePath, "*").matches(basePath.resolve(".foo")))
    assert(Glob(basePath, AnyPath).matches(basePath.resolve(".foo")))
    assert(!Glob(basePath, "[!.]*").matches(basePath.resolve(".foo")))
    assert(Glob(basePath, "[!.]*").matches(basePath.resolve("foo")))
    assert(Glob(basePath, "?*").matches(basePath.resolve(".foo")))
  }
  they should "be excluded with filter" in {
    val noHiddenFiles = Glob(basePath, "[!.]*")
    assert(!noHiddenFiles.matches(basePath.resolve(".foo")))
    assert(noHiddenFiles.matches(basePath.resolve("foo")))
    assert(!noHiddenFiles.matches(basePath.resolve(".f")))
    assert(noHiddenFiles.matches(basePath.resolve("f")))
    val noHiddenScalaFiles = Glob(basePath, "[!.]*.scala")
    assert(noHiddenScalaFiles.matches(basePath.resolve("Foo.scala")))
    assert(!noHiddenScalaFiles.matches(basePath.resolve(".Foo.scala")))
    assert(noHiddenScalaFiles.matches(basePath.resolve("a.scala")))
  }
  they should "be excluded with regex filter" in {
    val noHiddenFiles = Glob(basePath, "^[^.].*".r)
    assert(!noHiddenFiles.matches(basePath.resolve(".foo")))
    assert(noHiddenFiles.matches(basePath.resolve("foo")))
    assert(!noHiddenFiles.matches(basePath.resolve(".f")))
    assert(noHiddenFiles.matches(basePath.resolve("f")))
    val noHiddenScalaFiles = Glob(basePath, "^[^.].*[.]scala$".r)
    assert(noHiddenScalaFiles.matches(basePath.resolve("Foo.scala")))
    assert(!noHiddenScalaFiles.matches(basePath.resolve(".Foo.scala")))
    assert(noHiddenScalaFiles.matches(basePath.resolve("a.scala")))
    val altNoHiddenScalaFiles = Glob(basePath, "^[^.].*\\.scala$".r)
    assert(altNoHiddenScalaFiles.matches(basePath.resolve("Foo.scala")))
    assert(!altNoHiddenScalaFiles.matches(basePath.resolve(".Foo.scala")))
    assert(altNoHiddenScalaFiles.matches(basePath.resolve("a.scala")))
  }
}
