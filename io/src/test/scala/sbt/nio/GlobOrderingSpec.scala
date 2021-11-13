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

import java.io.File

import org.scalatest.flatspec.AnyFlatSpec
import sbt.io.{ IO, SimpleFilter }
import sbt.nio.file.RelativeGlob.Matcher
import sbt.nio.file._
import sbt.nio.file.syntax._

import scala.collection.JavaConverters._

class GlobOrderingSpec extends AnyFlatSpec {
  "Globs" should "be ordered" in IO.withTemporaryDirectory { dir =>
    val subdir = new File(dir, "subdir")
    assert(Seq(Glob(subdir), Glob(dir)).sorted == Seq(Glob(dir), Glob(subdir)))
  }
  they should "fall back on depth" in IO.withTemporaryDirectory { dir =>
    val recursive = Glob(dir, **)
    val nonRecursive = Glob(dir)
    assert(Seq(nonRecursive, recursive).sorted == Seq(recursive, nonRecursive))
  }
  they should "not stack overflow" in IO.withTemporaryDirectory { dir =>
    val exact = Glob(dir.toPath.resolve("foo"))
    val fullFile =
      sbt.internal.nio.Globs(dir.toPath / "foo", recursive = true, sbt.io.HiddenFileFilter)
    assert(Seq(exact, fullFile).sorted == Seq(exact, fullFile))
  }
  they should "not violate sorting contract" in IO.withTemporaryDirectory { dir =>
    val globs = Seq(
      **,
      ** / "foo",
      ** / * / "bar",
      ** / "foo" / "bar",
      ** / Matcher.or(Matcher("foo"), Matcher("bar")),
      ** / Matcher.and(Matcher("foo"), Matcher("bar")),
      ** / Matcher.not(Matcher("foo")),
      ** / Matcher.not(Matcher.and(Matcher("foo"), Matcher("bar"))),
      ** / Matcher(_.contains("foo")),
      ** / "foo" / Matcher(_.contains("bar")),
      ** / "foo" / Matcher.not(Matcher(_.contains("bar"))),
      ** / "foo" / "*.scala",
      ** / **,
      ** / *,
      (** / "foo") / * / "*.scala",
      (** / "foo") / * / "*.scala*",
      (** / "foo") / ** / "*.scala*",
      Glob(dir.toPath.resolve("foo")),
      Glob(dir.toPath.resolve("bar")),
      Glob(dir.toPath.resolve("bar").resolve("baz")),
      sbt.internal.nio.Globs(dir.toPath, recursive = false, sbt.io.AllPassFilter),
      sbt.internal.nio.Globs(dir.toPath, recursive = false, new SimpleFilter(_.contains("bar"))),
      sbt.internal.nio.Globs(dir.toPath, recursive = true, new SimpleFilter(_.contains("baz"))),
      sbt.internal.nio.Globs(dir.toPath, recursive = true, sbt.io.HiddenFileFilter),
      sbt.internal.nio.Globs(dir.toPath / "foo", recursive = true, sbt.io.HiddenFileFilter),
      sbt.internal.nio.Globs(dir.toPath, recursive = true, sbt.io.NothingFilter),
      sbt.internal.nio.Globs(dir.toPath, recursive = true, new SimpleFilter(_.contains("foo"))),
      Glob(dir.toPath / "scala", ** / "*.scala"),
      Glob(dir.toPath / "java", ** / "*.java"),
      Glob(dir.toPath / "scala", ** / "*.java"),
    )
    val javaGlobs = new java.util.ArrayList((globs ++ globs ++ globs).asJava)
    1 to 1000 foreach { _ =>
      java.util.Collections.shuffle(javaGlobs)
      javaGlobs.asScala.sorted
    }
  }
}
