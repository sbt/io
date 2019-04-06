package sbt.nio

import java.nio.file._

import org.scalatest.FlatSpec
import sbt.io.syntax._
import sbt.io.{ AllPassFilter, IO }

class FileTreeViewSpec extends FlatSpec {
  val view = FileTreeView.DEFAULT_NIO
  "FileTreeView" should "return the source root with depth == -1" in IO.withTemporaryDirectory {
    dir =>
      assert(view.list(dir.toGlob).map(_._1) == Seq(dir.toPath))
  }
  "FileTreeView" should "not return the source root with depth >= 0" in IO.withTemporaryDirectory {
    dir =>
      assert(view.list(Glob(dir.toPath, (1, 10), AllPass)).isEmpty)
  }
  "FileTreeView" should "get recursive files" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectory(dir.toPath.resolve("subdir"))
    val nestedSubdir = Files.createDirectory(subdir.resolve("nested-subdir"))
    val file = Files.createFile(nestedSubdir.resolve("file"))
    assert(
      view.list(dir ** AllPassFilter).collect { case (p, a) if !a.isDirectory => p } == Seq(file))
  }
}
