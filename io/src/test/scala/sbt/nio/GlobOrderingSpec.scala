package sbt.nio

import java.io.File

import org.scalatest.FlatSpec
import sbt.io.IO
import sbt.nio.file.{ Glob, RecursiveGlob }

class GlobOrderingSpec extends FlatSpec {
  "Globs" should "be ordered" in IO.withTemporaryDirectory { dir =>
    val subdir = new File(dir, "subdir")
    assert(Seq(Glob(subdir), Glob(dir)).sorted == Seq(Glob(dir), Glob(subdir)))
  }
  they should "fall back on depth" in IO.withTemporaryDirectory { dir =>
    val recursive = Glob(dir, RecursiveGlob)
    val nonRecursive = Glob(dir)
    assert(Seq(nonRecursive, recursive).sorted == Seq(recursive, nonRecursive))
  }
}
