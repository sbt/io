package sbt.io

import java.io.File
import java.nio.file.Files
import scala.collection.mutable

import org.scalatest.{ FlatSpec, Matchers }

object PathFinderSpec {
  implicit class FileOps(val file: File) extends AnyVal {
    def all(implicit handler: (File, FileFilter, mutable.Set[File]) => Unit): Seq[File] =
      PathFinder(file).globRecursive(AllPassFilter, handler).get()
  }
}
trait PathFinderSpec extends FlatSpec with Matchers {
  import PathFinderSpec._
  implicit def handler: (File, FileFilter, mutable.Set[File]) => Unit
  "PathFinder" should "find the files in a directory" in IO.withTemporaryDirectory { dir =>
    val foo = Files.createTempFile(dir.toPath, "foo", "").toFile
    val bar = Files.createTempFile(dir.toPath, "bar", "").toFile
    dir.all.toSet shouldBe Set(dir, foo, bar)
  }
  it should "find children of subdirectories" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createTempDirectory(dir.toPath, "subdir")
    val foo = Files.createTempFile(subdir, "foo", "").toFile
    dir.all.toSet shouldBe Set(dir, subdir.toFile, foo)
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
      dir.all.toSet shouldBe Set(dir, link.toFile, link.resolve(foo.getFileName).toFile)
    }
  }
  it should "include the base directory" in IO.withTemporaryDirectory { dir =>
    val file = Files.createFile(dir.toPath.resolve("file")).toFile
    dir.all.toSet shouldBe Set(dir, file)
  }
}
class NioPathFinderSpec extends PathFinderSpec {
  override def handler: (File, FileFilter, mutable.Set[File]) => Unit =
    DescendantOrSelfPathFinder.nio
}
class DefaultPathFinderSpec extends PathFinderSpec {
  override def handler: (File, FileFilter, mutable.Set[File]) => Unit =
    DescendantOrSelfPathFinder.default
}
