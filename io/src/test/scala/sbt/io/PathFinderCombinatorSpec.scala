package sbt.io

import java.nio.file.Files

import org.scalatest.FlatSpec
import sbt.io.syntax._

class PathFinderCombinatorSpec extends FlatSpec {
  "PathFinderCombinator" should "provide extension methods for File" in IO.withTemporaryDirectory {
    dir =>
      val file = Files.createFile(dir.toPath.resolve("file")).toFile
      assert((dir +++ file).get() == Seq(dir, file))
      assert(((dir: PathFinder.Combinator) +++ file).get() == Seq(dir, file))
      assert(((dir: PathFinder) +++ file).get() == Seq(dir, file))
  }
}
