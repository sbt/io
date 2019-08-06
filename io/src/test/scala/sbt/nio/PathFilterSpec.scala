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

import java.nio.file.{ Files, LinkOption, Path }

import org.scalatest.FlatSpec
import sbt.io.IO
import sbt.nio.file.syntax._
import sbt.nio.file._

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
      if (isWin) {
        Files.setAttribute(path, "dos:hidden", java.lang.Boolean.TRUE, LinkOption.NOFOLLOW_LINKS)
      } else path
  }
}
import sbt.nio.PathFilterSpec._
class PathFilterSpec extends FlatSpec {
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
    assert(!(scalaFilter && NotHiddenFilter).accept(hidden))
    assert((scalaFilter && NotHiddenFilter).accept(foo))
    assert(!(scalaFilter && !HiddenFilter).accept(hidden))
    assert((scalaFilter && !HiddenFilter).accept(foo))
    assert((scalaFilter && HiddenFilter).accept(hidden))
    assert(!(scalaFilter && HiddenFilter).accept(foo))
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
  they should "convert file filters" in IO.withTemporaryDirectory { dir =>
    val hiddenFileFilter: PathFilter = sbt.io.HiddenFileFilter
    val notHiddenFileFilter: PathFilter = -sbt.io.HiddenFileFilter
    val dirPath = dir.toPath
    val hidden = Files.createFile(dirPath / ".hidden.scala").setHidden()
    val regular = Files.createFile(dirPath / "foo.scala")
    assert(hiddenFileFilter.accept(hidden))
    assert((!hiddenFileFilter).accept(regular))
    assert(notHiddenFileFilter.accept(regular))
    assert((!notHiddenFileFilter).accept(hidden))

    val directoryFilterAndHidden: PathFilter = sbt.io.DirectoryFilter && sbt.io.HiddenFileFilter
    val hiddenDir = Files.createDirectories(dirPath / ".hidden").setHidden()
    assert(directoryFilterAndHidden.accept(hiddenDir) == !isWin)
    assert(!directoryFilterAndHidden.accept(dirPath))
    assert(!directoryFilterAndHidden.accept(hidden))

    val directoryFilterOrHidden: PathFilter = sbt.io.DirectoryFilter || sbt.io.HiddenFileFilter
    assert(directoryFilterOrHidden.accept(hiddenDir))
    assert(directoryFilterOrHidden.accept(dirPath))
    assert(directoryFilterOrHidden.accept(hidden))
  }
}
