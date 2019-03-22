package sbt.io

import java.io.File

import org.scalatest.FlatSpec
import syntax._

class GlobOrderingSpec extends FlatSpec {
  "Globs" should "be ordered" in IO.withTemporaryDirectory { dir =>
    val subdir = new File(dir, "subdir")
    assert(Seq(subdir.toGlob, dir.toGlob).sorted == Seq(dir.toGlob, subdir.toGlob))
  }
  they should "fall back on depth" in IO.withTemporaryDirectory { dir =>
    val recursive = dir.toGlob.withMaxDepth(Int.MaxValue)
    val nonRecursive = dir.toGlob.withMaxDepth(0)
    assert(Seq(nonRecursive, recursive).sorted == Seq(recursive, nonRecursive))
  }
}
