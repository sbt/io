/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.io

import java.io.File
import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sbt.io.{ AllPassFilter, NothingFilter, SimpleFileFilter }

@deprecated("Source has been replaced by Glob.", "1.3.0")
class SourceSpec extends AnyFlatSpec with Matchers {
  it should "accept recursive paths" in {
    val source = new Source(new File("/foo"), AllPassFilter, NothingFilter, true)
    source.accept(Paths.get("/foo/bar/baz")) shouldBe true
  }
  it should "reject subdirectories without recursive flag" in {
    val source = new Source(new File("/foo"), AllPassFilter, NothingFilter, false)
    source.accept(Paths.get("/foo/bar/baz")) shouldBe false
  }
  it should "apply include filter" in {
    val source = new Source(
      new File("/foo"),
      new SimpleFileFilter(_.toString.endsWith(".scala")),
      NothingFilter,
      true
    )
    source.accept(Paths.get("/foo/bar/baz.scala")) shouldBe true
    source.accept(Paths.get("/foo/bar/baz.java")) shouldBe false
  }
  it should "apply exclude filter" in {
    val source = new Source(
      new File("/foo"),
      new SimpleFileFilter(_.toString.endsWith(".scala")),
      new SimpleFileFilter(_ == sbt.io.syntax.file("/foo/bar/buzz.scala")),
      true
    )
    source.accept(Paths.get("/foo/bar/baz.scala")) shouldBe true
    source.accept(Paths.get("/foo/bar/buzz.scala")) shouldBe false
  }
  it should "override equals/hashcode" in {
    val source = new Source(new File("foo"), AllPassFilter, NothingFilter, true)
    val copy = new Source(new File("foo"), AllPassFilter, NothingFilter, true)
    assert(source == copy && copy == source)
    assert(source.hashCode == copy.hashCode)
    val others = Seq(
      new Source(new File("bar"), AllPassFilter, NothingFilter, true),
      new Source(new File("foo"), NothingFilter, NothingFilter, true),
      new Source(new File("foo"), AllPassFilter, AllPassFilter, true),
      new Source(new File("foo"), AllPassFilter, NothingFilter, false),
      new Object
    )
    others foreach { src =>
      assert(source != src && src != source)
      assert(source.hashCode != src.hashCode)
    }
  }
}
