/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.nio

import java.io.File
import java.nio.file.Files

import org.scalatest.FlatSpec
import sbt.io._
import sbt.nio.TestHelpers._
import sbt.nio.file.RelativeGlob.{ Matcher, NoPath }
import sbt.nio.file._

class GlobsSpec extends FlatSpec {
  "FullFileGlob" should "apply exact name filters" in {
    assert(Globs(basePath, recursive = true, "foo") == Glob(basePath, ** / "foo"))
    assert(Globs(basePath, recursive = false, "foo") == Glob(basePath, "foo"))
    assert(Globs(basePath, recursive = true, "foo") / "bar" == Glob(basePath, ** / "foo" / "bar"))
  }
  it should "apply extension filters" in {
    assert(Globs(basePath, recursive = true, "*.scala") == Glob(basePath, ** / "*.scala"))
    assert(Globs(basePath, recursive = false, "*.scala") == Glob(basePath, "*.scala"))
    val filter: FileFilter = ("*.scala": FileFilter) || "*.java"
    assert(Globs(basePath, recursive = false, filter) == Glob(basePath, "*.{scala,java}"))
  }
  it should "apply prefix filters" in {
    assert(Globs(basePath, recursive = true, "*foo") == Glob(basePath, ** / "*foo"))
    assert(Globs(basePath, recursive = false, "*foo") == Glob(basePath, "*foo"))
    assert(Globs(basePath, recursive = true, "*foo") / "bar" == Glob(basePath, ** / "*foo" / "bar"))
  }
  it should "apply suffix filters" in {
    assert(Globs(basePath, recursive = true, "foo*") == Glob(basePath, ** / "foo*"))
    assert(Globs(basePath, recursive = false, "foo*") == Glob(basePath, "foo*"))
    assert(Globs(basePath, recursive = true, "foo*") / "bar" == Glob(basePath, ** / "foo*" / "bar"))
  }
  it should "apply pattern filters" in {
    val patternFilter: FileFilter = "foo*bar*baz"
    val file = new File("fooxybarzwefwebaz")
    assert(patternFilter.accept(file))
    assert(Globs(basePath, recursive = true, patternFilter).matches(basePath.resolve(file.toPath)))
  }
  it should "apply simple filters" in {
    val simpleFilter: SimpleFilter = new SimpleFilter(_.startsWith("foo"))
    val file = new File("fooxybarzwefwebaz")
    assert(simpleFilter.accept(file))
    assert(Globs(basePath, recursive = true, simpleFilter).matches(basePath.resolve(file.toPath)))

  }
  it should "apply not filters" in {
    val filter: FileFilter = "foo"
    val notFilter: FileFilter = -filter
    val glob = Globs(basePath, recursive = true, notFilter)
    assert(!glob.matches(basePath.resolve("foo")))
    assert(glob.matches(basePath.resolve("fooo")))

    val altNotFilter: FileFilter = AllPassFilter -- filter
    val altGlob = Globs(basePath, recursive = true, altNotFilter)
    assert(!altGlob.matches(basePath.resolve("foo")))
    assert(altGlob.matches(basePath.resolve("fooo")))

    assert(Globs(basePath, recursive = false, filter -- NothingFilter) == Glob(basePath, "foo"))
  }
  it should "apply and filters" in {
    val fooFilter: NameFilter = "foo*"
    val fooBarFilter: NameFilter = "*foobar*"
    val glob = Globs(basePath, recursive = false, fooFilter & fooBarFilter)
    val altGlob = Globs(basePath, recursive = false, fooFilter && fooBarFilter)
    assert(glob == Glob(basePath, Matcher.and(Matcher("foo*"), Matcher("*foobar*"))))
    assert(altGlob == Glob(basePath, Matcher.and(Matcher("foo*"), Matcher("*foobar*"))))
    assert(glob.matches(basePath.resolve("foobarbaz")))
    assert(!glob.matches(basePath.resolve("barfoobarbaz")))
    assert(altGlob.matches(basePath.resolve("foobarbaz")))
    assert(!altGlob.matches(basePath.resolve("barfoobarbaz")))

    val notNothingGlob = Globs(basePath, recursive = false, fooFilter & -NothingFilter)
    val altNotNothingGlob = Globs(basePath, recursive = false, fooFilter && -NothingFilter)
    assert(notNothingGlob == Glob(basePath, "foo*"))
    assert(altNotNothingGlob == Glob(basePath, "foo*"))
    val nothingGlob = Globs(basePath, recursive = false, fooFilter & NothingFilter)
    val altNothingGlob = Globs(basePath, recursive = false, fooFilter && NothingFilter)
    assert(nothingGlob == Glob(basePath, NoPath))
    assert(altNothingGlob == Glob(basePath, NoPath))
  }
  it should "apply or filters" in {
    val scalaFilter: NameFilter = "*.scala"
    val javaFilter: NameFilter = "*.java"
    val glob = Globs(basePath, recursive = false, scalaFilter | javaFilter)
    val alternateGlob = Globs(basePath, recursive = false, scalaFilter || javaFilter)
    assert(glob == Glob(basePath, "*.{scala,java}"))
    assert(alternateGlob == Glob(basePath, "*.{scala,java}"))
    val allOrGlob = Globs(basePath, recursive = false, scalaFilter | AllPassFilter)
    val altAllOrGlob = Globs(basePath, recursive = false, scalaFilter || AllPassFilter)
    assert(allOrGlob == Glob(basePath, AnyPath))
    assert(altAllOrGlob == Glob(basePath, AnyPath))
    val notOrGlob = Globs(basePath, recursive = false, scalaFilter | NothingFilter)
    val altNotOrGlob = Globs(basePath, recursive = false, scalaFilter || NothingFilter)
    assert(notOrGlob == Glob(basePath, "*.scala"))
    assert(altNotOrGlob == Glob(basePath, "*.scala"))
  }
  it should "apply arbitrary filters" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val file = Files.createFile(dirPath.resolve("file"))
    val subdir = Files.createDirectories(dirPath.resolve("subdir"))
    val subFile = subdir.resolve("file")

    assert(Globs(dirPath, recursive = false, -DirectoryFilter).matches(file))
    assert(!Globs(dirPath, recursive = false, -DirectoryFilter).matches(subdir))
    assert(!Globs(dirPath, recursive = false, -DirectoryFilter).matches(subFile))

    assert(Globs(dirPath, recursive = true, -DirectoryFilter).matches(file))
    assert(!Globs(dirPath, recursive = true, -DirectoryFilter).matches(subdir))
    assert(Globs(dirPath, recursive = true, -DirectoryFilter).matches(subFile))
  }
  it should "apply anonymous filters" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val file = Files.createFile(dirPath.resolve("file.template"))
    val regex = ".*\\.template".r
    val filter = new NameFilter {
      override def accept(name: String): Boolean = regex.pattern.matcher(name).matches()
    }
    assert(Globs(dirPath, recursive = false, filter).matches(file))
  }
  it should "apply and filter with not hidden file filter" in {
    val filter = new ExtensionFilter("java", "scala") && -HiddenFileFilter
    val glob = Globs(basePath, recursive = true, filter)
    assert(glob.matches(basePath.resolve("foo").resolve("bar.scala")))
  }
  it should "apply and filter with not filter" in {
    val filter = new ExtensionFilter("java", "scala") && -new PrefixFilter("bar")
    val glob = Globs(basePath, recursive = true, filter)
    assert(!glob.matches(basePath.resolve("foo").resolve("bar.scala")))
    assert(glob.matches(basePath.resolve("foo").resolve("baz.java")))
  }
  it should "apply or with name filters" in {
    val excludeFilter: FileFilter = ("Baz.scala": NameFilter) || "Bar.scala"
    val includeFilter: NameFilter = "*.scala"
    val filter = includeFilter -- excludeFilter
    val glob = Globs(basePath, recursive = true, filter)
    assert(glob.matches(basePath.resolve("foo").resolve("Foo.scala")))
    assert(!glob.matches(basePath.resolve("foo").resolve("Bar.scala")))
  }
  "hidden files" should "be included by default" in {
    val glob = Globs(basePath, recursive = true, "*.scala")
    assert(glob.matches(basePath.resolve("foo").resolve("Foo.scala")))
    assert(glob.matches(basePath.resolve("foo").resolve("bar").resolve(".Bar.scala")))
  }
  they should "be excluded by filter" in {
    val glob = Globs(basePath, recursive = true, ("*.scala": NameFilter) -- HiddenFileFilter)
    assert(glob.matches(basePath.resolve("foo").resolve("Foo.scala")))
    assert(
      scala.util.Properties.isWin ||
        !glob.matches(basePath.resolve("foo").resolve("bar").resolve(".Bar.scala"))
    )
  }
}
