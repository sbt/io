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

import java.nio.file.{ Files, Path }

import org.scalatest.flatspec.AnyFlatSpec
import sbt.io.IO
import sbt.nio.file._
import sbt.nio.file.syntax._

object PathFilterSpec {
  implicit class PathFilterOps(val pathFilter: PathFilter) extends AnyVal {
    def accept(path: Path): Boolean = FileAttributes(path) match {
      case Left(_)  => pathFilter.accept(path, FileAttributes.NonExistent)
      case Right(a) => pathFilter.accept(path, a)
    }
  }
  private val isWin = scala.util.Properties.isWin
  implicit class PathOps(val path: Path) extends AnyVal {
    def setHidden(): Path =
      if (isWin) Files.setAttribute(path, "dos:hidden", true) else path
  }
}
import sbt.nio.PathFilterSpec._
class PathFilterSpec extends AnyFlatSpec {
  "PathFilters" should "accept files" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val foo = Files.createFile(dirPath / "foo")
    val fooTxt = dirPath / "foo.txt"
    val fooScala = Files.createFile(Files.createDirectories(dirPath / "bar") / "foo.scala")
    assert(AllPass.accept(foo))
    assert(AllPass.accept(fooTxt))
    assert(AllPass.accept(fooScala))
    assert(!(!AllPass).accept(foo))
    assert(!(!AllPass).accept(fooTxt))
    assert(!(!AllPass).accept(fooScala))
  }
  they should "exclude files" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    assert(!NoPass.accept(Files.createFile(dirPath / "foo")))
    assert(!NoPass.accept(dirPath / "foo.txt"))
    assert(!NoPass.accept(Files.createFile(Files.createDirectories(dirPath / "bar") / "foo.scala")))
  }
  they should "combine filters with &&" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val hidden = Files.createFile(dirPath / ".hidden.scala").setHidden()
    val foo = Files.createFile(dirPath / "foo.scala")
    val scalaFilter = PathFilter(dirPath.toGlob / "*.scala")
    assert(scalaFilter.accept(hidden))
    assert(scalaFilter.accept(foo))
    assert(!(scalaFilter && IsNotHidden).accept(hidden))
    assert((scalaFilter && IsNotHidden).accept(foo))
    assert(!(scalaFilter && !IsHidden).accept(hidden))
    assert((scalaFilter && !IsHidden).accept(foo))
    assert((scalaFilter && IsHidden).accept(hidden))
    assert(!(scalaFilter && IsHidden).accept(foo))
  }
  they should "combine filters with ||" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val foo = Files.createFile(dirPath / "foo.scala")
    val bar = Files.createFile(dirPath / "bar.java")
    val scalaFilter = PathFilter(dirPath.toGlob / "*.scala")
    val javaFilter = PathFilter(dirPath.toGlob / "*.java")
    assert(scalaFilter.accept(foo))
    assert(!scalaFilter.accept(bar))
    assert(!javaFilter.accept(foo))
    assert(javaFilter.accept(bar))
    assert((scalaFilter || javaFilter).accept(foo))
    assert((scalaFilter || javaFilter).accept(bar))
  }
  they should "combine glob strings" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val subdir = Files.createDirectories(dirPath / "subdir")
    val nested = Files.createDirectories(subdir / "nested")
    val bar = Files.createFile(subdir / "bar.txt")
    val baz = Files.createFile(subdir / "abcbazefg")
    val foo = Files.createFile(nested / "foo.md")
    val filter = AllPass && "**/{*.txt,*baz*}"
    assert(FileTreeView.default.list(dirPath.toGlob / **, filter).map(_._1).toSet == Set(bar, baz))
    assert(
      FileTreeView.default.list(dirPath.toGlob / **, !filter).map(_._1).toSet ==
        Set(subdir, nested, foo)
    )
  }
  they should "combine file filters" in IO.withTemporaryDirectory { dir =>
    val notHiddenFileFilter: PathFilter = !sbt.io.HiddenFileFilter
    val hiddenFileFilter: PathFilter = sbt.io.HiddenFileFilter
    val dirPath = dir.toPath
    val hidden = Files.createFile(dirPath / ".hidden.scala").setHidden()
    val regular = Files.createFile(dirPath / "foo.scala")
    assert(hiddenFileFilter.accept(hidden))
    assert((!hiddenFileFilter).accept(regular))
    assert(notHiddenFileFilter.accept(regular))
    assert((!notHiddenFileFilter).accept(hidden))

    val directoryFilterAndHidden: PathFilter =
      sbt.io.DirectoryFilter.toNio && sbt.io.HiddenFileFilter
    val hiddenDir = Files.createDirectories(dirPath / ".hidden").setHidden()
    assert(directoryFilterAndHidden.accept(hiddenDir) == !isWin)
    assert(!directoryFilterAndHidden.accept(dirPath))
    assert(!directoryFilterAndHidden.accept(hidden))

    val directoryFilterOrHidden: PathFilter =
      sbt.io.DirectoryFilter.toNio || sbt.io.HiddenFileFilter
    assert(directoryFilterOrHidden.accept(hiddenDir))
    assert(directoryFilterOrHidden.accept(dirPath))
    assert(directoryFilterOrHidden.accept(hidden))
  }
  they should "combine Globs" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val foo = Files.createFile(dirPath / "foo.txt")
    val bar = Files.createFile(dirPath / "bar.txt")
    val txtGlob = dirPath.toGlob / "*.txt"
    val notBar = txtGlob && !bar.toGlob
    assert(notBar.accept(foo))
    assert(! { notBar.accept(bar) })
    assert(!(!notBar).accept(foo))
    assert((!notBar).accept(bar))
  }
  they should "negate" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val foo = Files.createFile(dirPath / "foo.txt")
    val hidden = Files.createFile(dirPath / ".hidden").setHidden()
    val filter = IsDirectory && !IsHidden
    val notFilter = !filter
    assert(notFilter == (!IsDirectory || IsHidden))
    assert(!filter.accept(foo))
    assert(!filter.accept(hidden))
    assert(filter.accept(dirPath))
    assert(notFilter.accept(foo))
    assert(notFilter.accept(hidden))
    assert(!notFilter.accept(dirPath))

    val tripleAndFilter = IsDirectory && !IsHidden && !(** / "*.txt")
    val notTripleAndFilter = !tripleAndFilter
    assert(notTripleAndFilter == (!IsDirectory || IsHidden || (** / "*.txt")))
    assert(!(tripleAndFilter.accept(foo)))
    assert(tripleAndFilter.accept(dirPath))
    assert(!(tripleAndFilter.accept(hidden)))
    assert(notTripleAndFilter.accept(foo))
    assert(notTripleAndFilter.accept(hidden))
    assert(!(notTripleAndFilter.accept(dirPath)))

    val tripleOrFilter = IsDirectory || IsHidden || (** / "*.txt")
    val notTripleOrFilter = !tripleOrFilter
    assert(notTripleOrFilter == (!IsDirectory && !IsHidden && !(** / "*.txt")))
    assert(tripleOrFilter.accept(foo))
    assert(tripleOrFilter.accept(hidden))
    assert(tripleOrFilter.accept(dirPath))
    assert(!(notTripleOrFilter.accept(foo)))
    assert(!(notTripleOrFilter.accept(hidden)))
    assert(!(notTripleOrFilter.accept(dirPath)))
  }
}
