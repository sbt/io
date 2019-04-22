package sbt.nio

import java.io.File
import java.nio.file.{ Files, Paths }

import org.scalatest.FlatSpec
import sbt.io.FileFilter._
import sbt.io.syntax._
import sbt.io.{ AllPassFilter, DirectoryFilter, IO, SimpleFileFilter }
import sbt.nio.TestHelpers._
import sbt.nio.file.Glob.RelativeGlobViewOption
import sbt.nio.file.RelativeGlob.{ *, ** }
import sbt.nio.file.syntax._
import sbt.nio.file.{ AnyPath, Glob, RecursiveGlob, RelativeGlob }

import scala.util.Try

class GlobSyntaxSpec extends FlatSpec {
  "path builders" should "use `*` and `**` objects" in {
    assert(Glob(basePath, AnyPath).matches(basePath.resolve("foo")))
    assert(!Glob(basePath, AnyPath).matches(basePath.resolve("foo").resolve("bar")))
    assert(!Glob(basePath, AnyPath).matches(basePath.getParent))
    assert(Glob(basePath, AnyPath) == Glob(basePath, AnyPath))
    assert(Glob(basePath, RecursiveGlob) == Glob(basePath, RecursiveGlob))
    assert(Glob(basePath, AnyPath / RecursiveGlob) == Glob(basePath, AnyPath / RecursiveGlob))
    assert(Glob(basePath, p"*/**") == Glob(basePath, AnyPath / RecursiveGlob))
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
    assert(Glob(basePath, p"**/*foo[a-d]b").matches(basePath.resolve("abcdefooab")))
    assert(
      Glob(basePath, p"**/*/*foo[a-d]b").matches(basePath.resolve("bar").resolve("abcdefooab")))
    assert(
      Glob(basePath, p"**/*/*foo[a-d]b")
        .matches(basePath.resolve("bar").resolve("baz").resolve("buzz").resolve("abcdefooab")))
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
      complexGlob.matches(
        basePath.resolve("foo").resolve("bar").resolve("buzz").resolve("foo.txt")))
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
      complexGlob.matches(
        basePath.resolve("foo").resolve("bar").resolve("buzz").resolve("foo.txt")))
    assert(complexGlob.matches(basePath.resolve("foo").resolve("bar").resolve("foo.txt")))
    assert(!complexGlob.matches(basePath.resolve("bar").resolve("foo.txt")))
  }
  they should "apply suffix filters" in {
    assert(Glob(basePath, "*foo").matches(basePath.resolve("foo")))
    assert(Glob(basePath, "*foo").matches(basePath.resolve("barfoo")))
    assert(!Glob(basePath, "*foo").matches(basePath.resolve("bar").resolve("abcdfoo")))
    val complexGlob = Glob(basePath, AnyPath / RecursiveGlob / AnyPath / "*foo")
    assert(
      complexGlob.matches(
        basePath.resolve("foo").resolve("bar").resolve("buzz").resolve("abcdfoo")))
    assert(complexGlob.matches(basePath.resolve("foo").resolve("bar").resolve("abcdfoo")))
    assert(!complexGlob.matches(basePath.resolve("bar").resolve("abcdfoo")))
  }
  they should "apply split filters" in {
    assert(Glob(basePath, "foo*bar").matches(basePath.resolve("foobar")))
    assert(Glob(basePath, "foo*bar").matches(basePath.resolve("fooabcbar")))
    assert(Glob(basePath.getParent, p"bar/foo*bar").matches(basePath.resolve("fooabcbar")))
    assert(Glob(basePath, RecursiveGlob / "foo*bar").matches(basePath.resolve("fooabcbar")))
    assert(
      Glob(basePath, RecursiveGlob / "foo*bar")
        .matches(basePath.resolve("baz").resolve("buzz").resolve("fooabcbar")))
  }
  they should "work with file syntax" in IO.withTemporaryDirectory { dir =>
    val file = basePath.toFile
    assert(file * AllPassFilter == Glob(basePath, AnyPath))
    assert(file ** AllPassFilter == Glob(basePath, RecursiveGlob))
    assert(file * "*.txt" == Glob(basePath, "*.txt"))
    assert(file ** "*.txt" == Glob(basePath, RecursiveGlob / "*.txt"))
    val simple = file ** new SimpleFileFilter(f => f == basePath.resolve("foo").toFile)
    assert(simple.matches(basePath.resolve("foo")))
    assert(!simple.matches(basePath.resolve("foox")))
    assert(file * "*.txt" == Glob(basePath, "*.txt"))
    assert(file * ("*.txt" || "*.md") == Glob(basePath, "*.{txt,md}"))
    assert(file * "foo.txt" == Glob(basePath, "foo.txt"))
    val subdir = Files.createDirectory(dir.toPath.resolve("base"))
    val nestedSubdir = dir / "base" / "subdir" / "nested-subdir"
    Files.createDirectories(nestedSubdir.toPath)
    assert((dir * DirectoryFilter).matches(subdir))
    assert(!(dir * -DirectoryFilter).matches(subdir))
    assert((dir ** DirectoryFilter).matches(subdir))
    assert(!(dir ** -DirectoryFilter).matches(subdir))
    assert(((dir / "base" / "subdir").toGlob / AnyPath).matches(nestedSubdir.toPath))
    assert(!(dir / "base" / "subdir" * -DirectoryFilter).matches(nestedSubdir.toPath))
    assert((dir / "base" / "subdir" * DirectoryFilter).matches(nestedSubdir.toPath))
    assert(!(dir / "base" ** -DirectoryFilter).matches(nestedSubdir.toPath))
    assert((dir / "base" ** DirectoryFilter).matches(nestedSubdir.toPath))
  }
  they should "convert strings" in {
    assert((p"$basePath/*": Glob) == Glob(basePath, AnyPath))
    assert((p"$basePath/**": Glob) == Glob(basePath, RecursiveGlob))
    assert((p"$basePath/*/*.txt": Glob) == Glob(basePath, AnyPath / "*.txt"))
    assert((p"$basePath/**/*.txt": Glob) == Glob(basePath, RecursiveGlob / "*.txt"))
    assert((p"$basePath/*/*/*.txt": Glob) == Glob(basePath, AnyPath / AnyPath / "*.txt"))
    assert((p"$basePath/*/**/*.txt": Glob) == Glob(basePath, AnyPath / RecursiveGlob / "*.txt"))
  }
  they should "handle escaped characters" in {
    assert((basePath + File.separator + "\\{": Glob).matches(basePath.resolve("{")))
    assert((basePath + File.separator + "\\(": Glob).matches(basePath.resolve("(")))
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
    assert(Glob(basePath, "foo.txt").toString == p"$basePath/foo.txt")
    assert(Glob(basePath, "*").toString == p"$basePath/*")
    assert((p"$basePath/*": Glob).toString == p"$basePath/*")
    // extensions
    assert(Glob(basePath, "*.txt").toString == p"$basePath/*.txt")

    assert(Glob(basePath, "*.{txt,md}").toString == p"$basePath/*.{txt,md}")
    assert(Glob(basePath.getParent, RelativeGlob("bar") / "baz").toString == p"$basePath/baz")
  }
  "syntax" should "work" in {
    assert(basePath / "foo" == basePath.resolve("foo"))
    assert(basePath.toGlob / AnyPath == Glob(basePath, AnyPath))
    assert(
      basePath.toGlob / RecursiveGlob / AnyPath / "*.txt" == Glob(
        basePath,
        RecursiveGlob / AnyPath / "*.txt"))
    assert(basePath.toGlob / * == Glob(basePath, AnyPath))
    assert(basePath.toGlob / ** / * / "*.txt" == Glob(basePath, RecursiveGlob / AnyPath / "*.txt"))
  }
  "file tree view params" should "work with relative paths" in {
    implicit val option: RelativeGlobViewOption = RelativeGlobViewOption.Ignore
    assert(Glob(p"./foo").fileTreeViewListParameters._3.matches(Paths.get("foo").toAbsolutePath))
    assert(
      Glob(p"./foo/*").fileTreeViewListParameters._3.matches(Paths.get("foo/bar").toAbsolutePath))
  }
}
