package sbt.nio

import java.nio.file.Files

import org.scalatest.FlatSpec
import sbt.io.syntax._
import sbt.io.{ AllPassFilter, IO, NothingFilter, PathFinder }

object GlobPathFinderSpec {
  implicit class PathFinderOps[P](val p: P)(implicit f: P => PathFinder) {
    def set: Set[File] = f(p).get().toSet
  }
}

class GlobPathFinderSpec extends FlatSpec {
  import GlobPathFinderSpec._
  "GlobPathFinder" should "provide the same results as Pathfinder" in IO.withTemporaryDirectory {
    dir =>
      assert(dir.get() == Seq(dir))
  }
  it should "work with globbing" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "foo.txt")
    file.createNewFile()
    assert(file.get() == Seq(file))
    assert((dir glob AllPassFilter).set == (PathFinder(dir) glob AllPassFilter).set)
    assert((dir glob NothingFilter).get() == Nil)
    assert((dir glob "foo.txt").get() == Seq(file))
    assert((dir glob "bar.txt").get() == Nil)
  }
  it should "work with recursive globbing" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectories(dir.toPath.resolve("subdir")).toFile
    val file = new File(subdir, "foo.txt")
    file.createNewFile()
    assert(file.get() == Seq(file))
    assert((dir ** AllPassFilter).set == (PathFinder(dir) ** AllPassFilter).set)
    assert((dir ** NothingFilter).get() == Nil)
    assert((dir ** "foo.txt").get() == Seq(file))
    assert((dir ** "bar.txt").get() == Nil)
    assert(
      dir.descendantsExcept(AllPassFilter, NothingFilter).get().toSet ==
        PathFinder(dir).descendantsExcept(AllPassFilter, NothingFilter).get().toSet)
  }
  it should "work with combiners" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectories(dir.toPath.resolve("subdir")).toFile
    val file = new File(subdir, "foo.txt")
    file.createNewFile()
    val combined = dir +++ file
    assert(combined.set == Set(dir, file))
    val excluded = dir.allPaths --- file
    assert(excluded.set == Set(dir, subdir))
  }
  it should "return an empty list for directories that do not exists" in {
    assert((file("/tmp/this/is/not/a/file") * AllPassFilter).get == Nil)
  }
  it should "implicitly build a glob" in IO.withTemporaryDirectory { dir =>
    // These use the FileBuilder extension class for file.
    assert(dir.toGlob == Glob(dir.toPath, (0, 0), AllPass))
    assert(dir * AllPassFilter == Glob(dir.toPath, (1, 1), AllPass))
    assert((dir glob AllPassFilter) == Glob(dir.toPath, (1, 1), AllPass))
    assert(dir ** AllPassFilter == Glob(dir.toPath, (1, Int.MaxValue), AllPass))
    assert((dir globRecursive AllPassFilter) == Glob(dir.toPath, (1, Int.MaxValue), AllPass))
  }
}
