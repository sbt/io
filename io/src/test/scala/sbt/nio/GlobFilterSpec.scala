package sbt.nio

import org.scalatest.FlatSpec
import sbt.io.{ GlobFilter, IO, NothingFilter }
import sbt.io.syntax._
import Glob._

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
    val glob = Glob(dir.toPath, (0, 1), AllPass)
    assert(glob.toFileFilter.accept(dir))
    assert(glob.toFileFilter.accept(file))
    assert(!glob.toFileFilter.accept(nestedFile))
    val nothingGlob = dir * NothingFilter
    Seq(dir, file, nestedFile).foreach(f => assert(!nothingGlob.toFileFilter.accept(f)))
  }
  it should "work with recursive globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = Glob(dir.toPath, (0, Int.MaxValue), AllPass)
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
}
