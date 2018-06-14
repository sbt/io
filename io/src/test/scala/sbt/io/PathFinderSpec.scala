package sbt.io

import java.nio.file.Files

import org.scalatest.{ FlatSpec, Matchers }

class PathFinderSpec extends FlatSpec with Matchers {
  "PathFinder" should "find the files in a directory" in IO.withTemporaryDirectory { dir =>
    val foo = Files.createTempFile(dir.toPath, "foo", "").toFile
    val bar = Files.createTempFile(dir.toPath, "bar", "").toFile
    PathFinder(dir).allPaths.get.toSet shouldBe Set(dir, foo, bar)
  }
  it should "find children of subdirectories" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createTempDirectory(dir.toPath, "subdir")
    val foo = Files.createTempFile(subdir, "foo", "").toFile
    PathFinder(dir).allPaths.get.toSet shouldBe Set(dir, subdir.toFile, foo)
  }
  it should "apply filter" in IO.withTemporaryDirectory { dir =>
    val foo = Files.createTempFile(dir.toPath, "foo", "").toFile
    Files.createTempFile(dir.toPath, "bar", "").toFile
    val include = new SimpleFilter(_.startsWith("foo"))
    PathFinder(dir).descendantsExcept(include, NothingFilter).get shouldBe Seq(foo)
  }
  it should "follow links" in IO.withTemporaryDirectory { dir =>
    IO.withTemporaryDirectory { otherDir =>
      val foo = Files.createTempFile(otherDir.toPath, "foo", "")
      val link = Files.createSymbolicLink(dir.toPath.resolve("link"), otherDir.toPath)
      PathFinder(dir).allPaths.get.toSet shouldBe Set(dir,
                                                      link.toFile,
                                                      link.resolve(foo.getFileName).toFile)
    }
  }
}
