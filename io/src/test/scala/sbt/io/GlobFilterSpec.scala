package sbt.io
import java.io.File
import java.nio.file.Files

import org.scalatest.{ FlatSpec, Matchers }
import sbt.internal.io.Source

import scala.util.Properties

class GlobFilterSpec extends FlatSpec with Matchers {
  "GlobFilter" should "work with *" in {
    assert(GlobFilter("*") == AllPassFilter)
  }
  it should "work with no *" in {
    assert(GlobFilter("foo.txt") == new ExactFilter("foo.txt"))
    assert(GlobFilter("foo.txt").accept("foo.txt"))
  }
  it should "work with simple extensions" in {
    assert(GlobFilter("*.txt") == new ExtensionFilter("txt"))
    assert(GlobFilter("*.txt").accept("foo.txt"))
  }
  it should "combine extensions" in {
    assert((GlobFilter("*.scala") && GlobFilter("*.java")) == new ExtensionFilter())
    assert((GlobFilter("*.scala") & GlobFilter("*.java")) == new ExtensionFilter())
    assert((GlobFilter("*.scala") || GlobFilter("*.java")) == new ExtensionFilter("scala", "java"))
    assert((GlobFilter("*.scala") | GlobFilter("*.java")) == new ExtensionFilter("scala", "java"))
    val scalaFilter = new ExtensionFilter("scala")
    assert((GlobFilter("*.scala") || GlobFilter("*.java") -- GlobFilter("*.java")) == scalaFilter)
    assert((GlobFilter("*.scala") || GlobFilter("*.java") - GlobFilter("*.java")) == scalaFilter)
    assert((GlobFilter("*.scala") -- ExistsFileFilter).accept(new File("foo.scala")))
    assert(!(GlobFilter("*.scala") && ExistsFileFilter).accept(new File("foo.scala")))
    assert((GlobFilter("*.scala") || DirectoryFilter).accept(new File(".")))
  }
  it should "work with patterns" in {
    val filter = GlobFilter("foo*.txt")
    assert(filter.accept(new File("foobar.txt")))
    assert(!filter.accept(new File("bar.txt")))
  }
}

object GlobFilterSyntaxSpec {
  implicit class PathFinderOps(val source: Source) {
    def pathSet: Set[java.nio.file.Path] = {
      val baseFinder = PathFinder(source.base)
      val filter = source.includeFilter -- source.excludeFilter
      val finder = if (source.recursive) baseFinder ** filter else baseFinder * filter
      finder.get().map(_.toPath).toSet
    }
  }
}
class GlobFilterSyntaxSpec extends FlatSpec with Matchers {
  import syntax._
  import GlobFilterSyntaxSpec._

  "**" should "collect all files" in IO.withTemporaryDirectory { dir =>
    val f = Files.createFile(dir.toPath.resolve("file"))
    val subdir = Files.createDirectories(dir.toPath.resolve("subdir"))
    val subdirFile = Files.createFile(subdir.resolve("file"))

    val globbedPaths = (dir / **).pathSet
    val expected = Set(dir.toPath, f, subdir, subdirFile)
    assert(globbedPaths == expected)
  }
  it should "filter by extension" in IO.withTemporaryDirectory { dir =>
    val baseScala = Files.createFile(dir.toPath.resolve("Base.scala"))
    val subdir = Files.createDirectories(dir.toPath.resolve("subdir"))
    val subScala = Files.createFile(subdir.resolve("Sub.scala"))
    val subJava = Files.createFile(subdir.resolve("Sub.java"))

    val globbedScalaPaths = (dir / ** / *.scala).pathSet
    val expectedScalaPaths = Set(baseScala, subScala)
    assert(globbedScalaPaths == expectedScalaPaths)

    val globbedJavaPaths = (dir / ** / *.java).pathSet
    val expectedJavaPaths = Set(subJava)
    assert(globbedJavaPaths == expectedJavaPaths)

    val globbedSourcePaths = (dir / ** / (*.scala | *.java)).pathSet
    val expectedSourcePaths = expectedJavaPaths ++ expectedScalaPaths
    assert(globbedSourcePaths == expectedSourcePaths)
  }
  "*" should "collect base files" in IO.withTemporaryDirectory { dir =>
    val baseScala = Files.createFile(dir.toPath.resolve("Base.scala"))
    val baseJava = Files.createFile(dir.toPath.resolve("Base.java"))
    val subdir = Files.createDirectories(dir.toPath.resolve("subdir"))
    val subScala = Files.createFile(subdir.resolve("Sub.scala"))
    val subJava = Files.createFile(subdir.resolve("Sub.java"))

    val globbedScalaPaths = (dir / *.scala).pathSet
    val expectedScalaPaths = Set(baseScala)
    assert(globbedScalaPaths == expectedScalaPaths)

    val globbedJavaPaths = (dir / *.java).pathSet
    val expectedJavaPaths = Set(baseJava)
    assert(globbedJavaPaths == expectedJavaPaths)

    val allPaths = (dir / **).pathSet
    val expectedAllPaths = Set(dir.toPath, subdir, baseScala, baseJava, subScala, subJava)
    assert(allPaths == expectedAllPaths)
  }
  it should "work with string components" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectories(dir.toPath.resolve("subdir"))
    val subScala = Files.createFile(subdir.resolve("Sub.scala"))
    val subJava = Files.createFile(subdir.resolve("Sub.java"))

    val globbedScalaPaths = (dir / "subdir" / *.scala).pathSet
    val expectedScalaPaths = Set(subScala)
    assert(globbedScalaPaths == expectedScalaPaths)

    val globbedJavaPaths = (dir / "subdir" / *.java).pathSet
    val expectedJavaPaths = Set(subJava)
    assert(globbedJavaPaths == expectedJavaPaths)

    val allPaths = (dir / "subdir" / *).pathSet
    val expectedAllPaths = Set(subScala, subJava)
    assert(allPaths == expectedAllPaths)
  }
  "strings" should "have same semantics as objects" in IO.withTemporaryDirectory { dir =>
    // windows doesn't like paths with "*" in them.
    if (!Properties.isWin) {
      val baseScala = Files.createFile(dir.toPath.resolve("Base.scala"))
      Files.createFile(dir.toPath.resolve("Base.java"))
      val subdir = Files.createDirectories(dir.toPath.resolve("subdir"))
      val subScala = Files.createFile(subdir.resolve("Sub.scala"))
      Files.createFile(subdir.resolve("Sub.java"))

      val globbedStringScalaPaths = (dir / "*.scala").toSource.pathSet
      val expectedScalaPaths = (dir / *.scala).pathSet
      assert(globbedStringScalaPaths == expectedScalaPaths)

      val globbedJavaPaths = (dir / "*.java").toSource.pathSet
      val expectedJavaPaths = (dir / *.java).pathSet
      assert(globbedJavaPaths == expectedJavaPaths)

      val allStringScalaPaths = (dir / "**" / "*.scala").toSource.pathSet
      val expectedAllPaths = Set(baseScala, subScala)
      assert(allStringScalaPaths == expectedAllPaths)
    }
  }
}
