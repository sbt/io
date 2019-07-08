/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

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
  it should "handle multiple globs" in IO.withTemporaryDirectory { dir =>
    val srcSubdir = Files.createDirectory(dir.toPath.resolve("src"))
    val mainSubdir = Files.createDirectory(srcSubdir.resolve("main"))
    val scalaSubdir = Files.createDirectory(mainSubdir.resolve("scala"))
    val exampleSubdir = Files.createDirectory(scalaSubdir.resolve("example"))
    val fooFile = Files.createFile(exampleSubdir.resolve("foo.scala"))
    val elevenSubdir = Files.createDirectory(mainSubdir.resolve("scala_2.11+"))
    val barFile = Files.createFile(elevenSubdir.resolve("bar.scala"))
    val jvmGlob = Glob(dir, "jvm")
    val srcGlob = Glob(dir, "src")

    val xs = FileTreeView
      .Ops(FileTreeView.default)
      .list(
        List(
          jvmGlob / "src" / "main" / "scala-2.12" / RecursiveGlob / "*.{scala,java}",
          jvmGlob / "src" / "main" / "scala" / RecursiveGlob / "*.{scala,java}",
          jvmGlob / "src" / "main" / "java" / RecursiveGlob / "*.{scala,java}",
          jvmGlob / "src" / "main" / "java" / RecursiveGlob / "*.txt",
          srcGlob / "main" / "scala-2.12" / RecursiveGlob / "*.{scala,java}",
          srcGlob / "main" / "scala" / RecursiveGlob / "*.{scala,java}",
          srcGlob / "main" / "scala_2.11+" / RecursiveGlob / "*.{scala,java}",
          srcGlob / "main" / "scala_2.13-" / RecursiveGlob / "*.{scala,java}",
          jvmGlob / AnyPath / "*.{scala,java}"
        )
      )
      .map { case (p, _) => p }
    assert(xs.contains(fooFile) && xs.contains(barFile))
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
