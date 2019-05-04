package sbt.nio

import java.nio.file.Paths

import org.scalatest.FlatSpec
import sbt.io.syntax._
import sbt.io.{ GlobFilter, IO, NothingFilter }
import sbt.nio.TestHelpers._
import sbt.nio.file.{ AnyPath, Glob, RecursiveGlob }

class GlobFilterSpec extends FlatSpec {
  "GlobAsFilter" should "work with simple files" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val filter = Glob(dir).toFileFilter
    assert(filter.accept(dir))
    assert(!filter.accept(file))
    assert(!filter.accept(nestedFile))
  }
  it should "work with globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = Glob(dir, AnyPath)
    assert(!glob.matches(dir.toPath))
    assert(glob.matches(file.toPath))
    assert(!glob.matches(nestedFile.toPath))
    val nothingGlob = dir * NothingFilter
    Seq(dir, file, nestedFile).foreach(f => assert(!nothingGlob.toFileFilter.accept(f)))
  }
  it should "work with recursive globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = Glob(dir, RecursiveGlob)
    assert(!glob.matches(dir.toPath))
    assert(glob.matches(file.toPath))
    assert(glob.matches(nestedFile.toPath))
    val nothingGlob = dir ** NothingFilter
    Seq(dir, file, nestedFile).foreach(f => assert(!nothingGlob.toFileFilter.accept(f)))
  }
  it should "work with complex name filters" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "build.sbt")
    val glob = dir * (GlobFilter("*.sbt") - ".sbt")
    assert(glob.matches(file.toPath))
  }
  it should "work with depth" in {
    val base = Paths.get("").toAbsolutePath.getRoot.resolve("foo").resolve("bar")
    assert(Glob(base, p"*/*.txt").matches(base.resolve("foo").resolve("bar.txt")))
    assert(!Glob(base, p"*/*/*.txt").matches(base.resolve("foo").resolve("bar.txt")))
    assert(Glob(base, p"*/*/*.txt").matches(base.resolve("foo").resolve("baz").resolve("bar.txt")))
    assert(!Glob(base, p"*/*/*.txt").matches(base.resolve("foo").resolve("baz").resolve("bar.tx")))
    assert(Glob(base, p"*/**/*.txt").matches(base.resolve("foo").resolve("bar.txt")))
    assert(
      Glob(base, p"*/**/*.txt")
        .matches(base.resolve("foo").resolve("bar").resolve("baz").resolve("bar.txt"))
    )
    assert(
      !Glob(base, p"*/**/*.txt")
        .matches(base.resolve("foo").resolve("bar").resolve("baz").resolve("bar.tx"))
    )
  }
}
