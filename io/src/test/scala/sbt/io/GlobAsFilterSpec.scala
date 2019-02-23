package sbt.io

import org.scalatest.FlatSpec
import syntax._

class GlobAsFilterSpec extends FlatSpec {
  "GlobAsFilter" should "work with simple files" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val filter = dir.toGlob.toFileFilter
    assert(filter.accept(dir))
    assert(!filter.accept(file))
    assert(!filter.accept(nestedFile))
    assert(!dir.toGlob.withFilter(NothingFilter).toFileFilter.accept(dir))
  }
  it should "work with globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = dir * AllPassFilter
    assert(!glob.toFileFilter(acceptBase = false).accept(dir))
    assert(glob.toFileFilter.accept(dir))
    assert(glob.toFileFilter.accept(file))
    assert(!glob.toFileFilter.accept(nestedFile))
    val nothingGlob = dir * NothingFilter
    Seq(dir, file, nestedFile).foreach(f => assert(!nothingGlob.toFileFilter.accept(f)))
  }
  it should "work with recursive globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = dir ** AllPassFilter
    assert(!glob.toFileFilter(acceptBase = false).accept(dir))
    assert(glob.toFileFilter.accept(dir))
    assert(glob.toFileFilter.accept(file))
    assert(glob.toFileFilter.accept(nestedFile))
    val nothingGlob = dir ** NothingFilter
    Seq(dir, file, nestedFile).foreach(f => assert(!nothingGlob.toFileFilter.accept(f)))
  }
}
