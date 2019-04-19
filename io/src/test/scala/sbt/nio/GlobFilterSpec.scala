package sbt.nio

import java.nio.file.Paths

import org.scalatest.FlatSpec
import sbt.io.{ GlobFilter, IO, NothingFilter }
import sbt.io.syntax._
import TestHelpers._
import sbt.nio
import sbt.nio.file.Glob
import sbt.nio.filters.{ AllPass, NoPass }
import sbt.nio.syntax._

class GlobFilterSpec extends FlatSpec {
  "GlobAsFilter" should "work with simple files" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val filter = dir.toGlob.toFileFilter
    assert(filter.accept(dir))
    assert(!filter.accept(file))
    assert(!filter.accept(nestedFile))
    assert(!Glob(dir.toPath, (0, 0), NoPass).filter.accept(dir.toPath))
  }
  it should "work with globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = nio.file.Glob(dir.toPath, (0, 1), AllPass)
    assert(glob.toFileFilter.accept(dir))
    assert(glob.toFileFilter.accept(file))
    assert(!glob.toFileFilter.accept(nestedFile))
    val nothingGlob = dir * NothingFilter
    Seq(dir, file, nestedFile).foreach(f => assert(!nothingGlob.toFileFilter.accept(f)))
  }
  it should "work with recursive globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = nio.file.Glob(dir.toPath, (0, Int.MaxValue), AllPass)
    assert(glob.toFileFilter.accept(dir))
    assert(glob.toFileFilter.accept(file))
    assert(glob.toFileFilter.accept(nestedFile))
    val nothingGlob = dir ** NothingFilter
    Seq(dir, file, nestedFile).foreach(f => assert(!nothingGlob.toFileFilter.accept(f)))
  }
  it should "work with complex name filters" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "build.sbt")
    val glob = dir * (GlobFilter("*.sbt") - ".sbt")
    assert(glob.filter.accept(file.toPath))
  }
  it should "work with depth" in {
    val base = Paths.get("").toAbsolutePath.getRoot.resolve("foo").resolve("bar")
    assert((base / p"*/*.txt").filter.accept(base.resolve("foo").resolve("bar.txt")))
    assert(!(base / p"*/*/*.txt").filter.accept(base.resolve("foo").resolve("bar.txt")))
    assert(
      (base / p"*/*/*.txt").filter
        .accept(base.resolve("foo").resolve("baz").resolve("bar.txt")))
    assert(
      !(base / p"*/*/*.txt").filter
        .accept(base.resolve("foo").resolve("baz").resolve("bar.tx")))
    assert((base / p"*/**/*.txt").filter.accept(base.resolve("foo").resolve("bar.txt")))
    assert(
      (base / p"*/**/*.txt").filter
        .accept(base.resolve("foo").resolve("bar").resolve("baz").resolve("bar.txt")))
    assert(
      !(base / p"*/**/*.txt").filter
        .accept(base.resolve("foo").resolve("bar").resolve("baz").resolve("bar.tx")))
  }
}
