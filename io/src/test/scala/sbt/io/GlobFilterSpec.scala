/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.io
import java.io.File

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GlobFilterSpec extends AnyFlatSpec with Matchers {
  "GlobFilter" should "work with *" in {
    assert(GlobFilter("*") == AllPassFilter)
  }
  it should "work with no *" in {
    assert(GlobFilter("foo.txt") == new ExactFilter("foo.txt"))
    assert(GlobFilter("foo.txt").accept("foo.txt"))
  }
  it should "work with simple extensions" in {
    assert(GlobFilter("*.txt") == new ExtensionFilter("txt"))
    assert(GlobFilter("*.txt").accept("foo.txt"))
  }
  it should "combine extensions" in {
    assert((GlobFilter("*.scala") && GlobFilter("*.java")) == new ExtensionFilter())
    assert((GlobFilter("*.scala") & GlobFilter("*.java")) == new ExtensionFilter())
    assert((GlobFilter("*.scala") || GlobFilter("*.java")) == new ExtensionFilter("scala", "java"))
    assert((GlobFilter("*.scala") | GlobFilter("*.java")) == new ExtensionFilter("scala", "java"))
    val scalaFilter = new ExtensionFilter("scala")
    assert((GlobFilter("*.scala") || GlobFilter("*.java") -- GlobFilter("*.java")) == scalaFilter)
    assert((GlobFilter("*.scala") || GlobFilter("*.java") - GlobFilter("*.java")) == scalaFilter)
    assert((GlobFilter("*.scala") -- ExistsFileFilter).accept(new File("foo.scala")))
    assert(!(GlobFilter("*.scala") && ExistsFileFilter).accept(new File("foo.scala")))
    assert((GlobFilter("*.scala") || DirectoryFilter).accept(new File(".")))
  }
  it should "work with patterns" in {
    val filter = GlobFilter("foo*.txt")
    assert(filter.accept(new File("foobar.txt")))
    assert(!filter.accept(new File("bar.txt")))
  }
  it should "work with trailing *" in {
    val filter = GlobFilter("foo*")
    assert(filter.isInstanceOf[PrefixFilter])
    assert(filter.accept(new File("foobar.txt")))
    assert(!filter.accept(new File("fobar.txt")))
  }
  it should "work with leading *" in {
    val filter = GlobFilter("*foo.txt")
    assert(filter.isInstanceOf[SuffixFilter])
    assert(filter.accept(new File("foo.txt")))
    assert(filter.accept(new File("afoo.txt")))
    assert(!filter.accept(new File("afoo.txta")))
  }
}
