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

import org.scalatest.flatspec.AnyFlatSpec
import sbt.io.IO
import sbt.nio.file.{
  **,
  AnyPath,
  IsDirectory,
  FileAttributes,
  FileTreeView,
  Glob,
  RecursiveGlob,
  IsRegularFile
}
import sbt.nio.file.syntax._

import scala.collection.mutable

class FileTreeViewSpec extends AnyFlatSpec {
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
  it should "list directories only once" in IO.withTemporaryDirectory { dir =>
    val file1 = Files.createFile(dir.toPath.resolve("file-1"))
    val file2 = Files.createFile(dir.toPath.resolve("file-2"))
    val listed = new mutable.ArrayBuffer[Path]
    val view: FileTreeView.Nio[FileAttributes] = (path: Path) => {
      val res = FileTreeView.default.list(path)
      listed += path
      res
    }
    val paths = view.list(Seq(Glob(file1), Glob(file2))).map(_._1)
    assert(paths.toSet == Set(file1, file2))
    assert(listed == Seq(file1.getParent))
  }
  it should "apply filters" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectories(dir.toPath.resolve("subdir"))
    val nested = Files.createDirectories(subdir.resolve("nested"))
    val subdirFile = Files.createFile(subdir.resolve("file"))
    val nestedFile = Files.createFile(nested.resolve("file"))
    val glob = Glob(dir.toPath, RecursiveGlob)

    val files = FileTreeView.default.list(glob, IsRegularFile)
    assert(files.map(_._1) == Seq(subdirFile, nestedFile))

    val directories = FileTreeView.default.list(glob, IsDirectory)
    assert(directories.map(_._1) == Seq(subdir, nested))
  }
  it should "handle exact file glob" in IO.withTemporaryDirectory { dir =>
    val file = Files.createFile(dir.toPath.resolve("file"))
    assert(FileTreeView.default.list(file.toGlob).map(_._1) == Seq(file))
  }
  it should "handle many exact file globs" in IO.withTemporaryDirectory { dir =>
    val random = new scala.util.Random()
    val n = 2000
    val nested = Files.createDirectories(dir.toPath / "subdir" / "nested")
    // TODO https://github.com/lampepfl/dotty/issues/13941
    // @tailrec
    def newRandomFile(): Path =
      try Files.createFile(nested / s"a${1 + random.nextInt(n)}")
      catch { case _: FileAlreadyExistsException => newRandomFile() }
    val randomFiles = (1 to Seq(10, n).min).map(_ => newRandomFile())
    val globs = (1 to n).map(i => dir.toGlob / "subdir" / "nested" / s"a$i") :+ Glob(dir, ** / "b*")
    assert(randomFiles.toSet == FileTreeView.default.list(globs).map(_._1).toSet)
  }
  it should "handle overlapping globs with exact file" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectories(dir.toPath / "subdir")
    val nested = Files.createDirectories(subdir / "nested")
    val a = Files.createFile(subdir / "a.txt")
    val b = Files.createFile(nested / "b.txt")
    val globs = Seq(Glob(subdir, "a.txt"), Glob(nested, RecursiveGlob))
    val queried = FileTreeView.default.list(globs).map(_._1).toSet
    assert(queried == Set(a, b))
  }
  it should "throw NoSuchFileException for non-existent directories" in IO.withTemporaryDirectory {
    dir =>
      intercept[NoSuchFileException](FileTreeView.nio.list(dir.toPath / "foo"))
      intercept[NoSuchFileException](FileTreeView.native.list(dir.toPath / "foo"))
  }
  it should "throw NotDirectoryException for regular files" in IO.withTemporaryDirectory { dir =>
    val file = Files.createFile(dir.toPath / "file")
    intercept[NotDirectoryException](FileTreeView.nio.list(file))
    intercept[NotDirectoryException](FileTreeView.native.list(file))
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
    assert(firstSubdirFoundFile.map(_.getParent).contains(firstSubdir))
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
