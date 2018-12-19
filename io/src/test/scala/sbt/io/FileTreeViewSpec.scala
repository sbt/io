package sbt.io

import java.nio.file._
import org.scalatest.FlatSpec
import sbt.io.FileTreeView.AllPass

class FileTreeViewSpec extends FlatSpec {
  val view = FileTreeView.DEFAULT
  "FileTreeView" should "return the source root with depth == -1" in IO.withTemporaryDirectory {
    dir =>
      assert(view.list(dir.toPath, -1, AllPass).map(_.toPath) == Seq(dir.toPath))
  }
  "FileTreeView" should "not return the source root with depth >= 0" in IO.withTemporaryDirectory {
    dir =>
      assert(view.list(dir.toPath, 0, AllPass).isEmpty)
      assert(view.list(dir.toPath, 10, AllPass).isEmpty)
  }
  "FileTreeView" should "get recursive files" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectory(dir.toPath.resolve("subdir"))
    val nestedSubdir = Files.createDirectory(subdir.resolve("nested-subdir"))
    val file = Files.createFile(nestedSubdir.resolve("file"))
    assert(view.list(dir.toPath, 3, (_: TypedPath).toPath == file).map(_.toPath) == Seq(file))
  }
}
