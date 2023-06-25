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

import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import sbt.nio.TestHelpers._
import sbt.nio.file.RelativeGlob
import sbt.nio.file.syntax._
import sbt.nio.file._
import sbt.nio.file.Glob.GlobOps

class PathSyntaxSpec extends AnyFlatSpec {
  "toGlob" should "work with absolute paths" in {
    assert(basePath.toGlob.matches(basePath))
    assert(!basePath.toGlob.matches(basePath.getParent))
    assert(!basePath.toGlob.matches(basePath / "foo"))
  }
  it should "work with relative paths" in {
    val base = Paths.get("foo") / "bar"
    val baseGlob = base.toGlob
    assert(baseGlob.matches(base))
    baseGlob match {
      case r: RelativeGlob =>
        val absoluteGlob = basePath.toGlob / r
        val absolutePath = basePath.resolve(base)
        assert(absoluteGlob.base == basePath.resolve(base))
        assert(absoluteGlob.matches(basePath.resolve(base)))
        val txtGlob = absoluteGlob / ** / "*.txt"
        assert(txtGlob.matches(absolutePath / "foo" / "bar" / "baz.txt"))
        assert(!txtGlob.matches(absolutePath))
        assert(!txtGlob.matches(absolutePath.getParent / "foo.txt"))
      case _ => throw new IllegalStateException("Relative path was not converted to relative glob")
    }
  }
  it should "work with empty paths" in {
    val empty = Paths.get("").toGlob match {
      case r: RelativeGlob => r
      case _ => throw new IllegalStateException("Relative path was not converted to relative glob")
    }
    val glob = basePath.toGlob / empty / ** / empty / empty / "*.txt"
    assert(glob.matches(basePath / "foo.txt"))
    assert(glob.matches(basePath / "foo" / "bar" / "baz.txt"))
  }
}
