package sbt.nio

import java.nio.file._

import org.scalatest.FlatSpec
import sbt.io.IO
import sbt.nio.file.{ AnyPath, FileAttributes, FileTreeView, Glob, RecursiveGlob }

import scala.collection.mutable

class FileTreeViewSpec extends FlatSpec {
  val view = FileTreeView.default
  "list" should "return the source root with depth == -1" in IO.withTemporaryDirectory { dir =>
    assert(view.list(dir.toPath.getParent).filter(_._1 == dir.toPath).map(_._1) == Seq(dir.toPath))
  }
  it should "not return the source root with depth >= 0" in IO.withTemporaryDirectory { dir =>
    assert(view.list(Glob(dir, AnyPath / RecursiveGlob)).isEmpty)
  }
  it should "get recursive files" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectory(dir.toPath.resolve("subdir"))
    val nestedSubdir = Files.createDirectory(subdir.resolve("nested-subdir"))
    val file = Files.createFile(nestedSubdir.resolve("file"))
    assert(
      view.list(Glob(dir, RecursiveGlob)).collect { case (p, a) if !a.isDirectory => p } == Seq(
        file
      )
    )
  }
  "iterator" should "be lazy" in IO.withTemporaryDirectory { dir =>
    val firstSubdir = Files.createDirectory(dir.toPath.resolve("first"))
    val firstSubdirFile = Files.createFile(firstSubdir.resolve("first-file"))
    val firstSubdirOtherFile = Files.createFile(firstSubdir.resolve("second-file"))
    val secondSubdir = Files.createDirectory(firstSubdir.resolve("second"))
    val secondSubdirFile = Files.createFile(secondSubdir.resolve("file"))
    object ListingFileTreeView extends FileTreeView.Nio[FileAttributes] {
      val listed = mutable.Set.empty[Path]
      override def list(path: Path): Seq[(Path, FileAttributes)] = {
        listed += path
        FileTreeView.default.list(path)
      }
    }
    val firstSubdirFoundFile =
      ListingFileTreeView.iterator(Glob(dir, RecursiveGlob)).collectFirst {
        case (p, a) if a.isRegularFile => p
      }
    assert(firstSubdirFoundFile.map(_.getParent) == Some(firstSubdir))
    assert(ListingFileTreeView.listed.toSet == Set(dir.toPath, firstSubdir))
    val allFiles = ListingFileTreeView
      .iterator(Glob(dir, RecursiveGlob))
      .collect {
        case (p, a) if a.isRegularFile => p
      }
      .toSet
    assert(allFiles == Set(firstSubdirFile, firstSubdirOtherFile, secondSubdirFile))
    assert(ListingFileTreeView.listed.toSet == Set(dir.toPath, firstSubdir, secondSubdir))
  }
}
