package sbt.io

import java.nio.file._

import org.scalatest.FlatSpec
import sbt.internal.io.FileTreeView
import sbt.internal.io.FileTreeView.AllPass
import sbt.io.syntax._

class FileTreeViewSpec extends FlatSpec {
  val view = FileTreeView.DEFAULT
  "FileTreeView" should "return the source root with depth == -1" in IO.withTemporaryDirectory {
    dir =>
      assert(view.list(dir.toGlob, AllPass).map(_._1) == Seq(dir.toPath))
  }
  "FileTreeView" should "not return the source root with depth >= 0" in IO.withTemporaryDirectory {
    dir =>
      assert(view.list(dir.toPath * AllPassFilter, AllPass).isEmpty)
      assert(view.list((dir.toPath * AllPassFilter).withDepth(10), AllPass).isEmpty)
  }
  "FileTreeView" should "get recursive files" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectory(dir.toPath.resolve("subdir"))
    val nestedSubdir = Files.createDirectory(subdir.resolve("nested-subdir"))
    val file = Files.createFile(nestedSubdir.resolve("file"))
    assert(view.list(dir ** AllPassFilter, _._2.isRegularFile).map(_._1) == Seq(file))
  }
}
