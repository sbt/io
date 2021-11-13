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

class CombinedFilterSpec extends AnyFlatSpec with Matchers {
  def endsWithTxt: String => Boolean = new Function[String, Boolean] {
    override def apply(string: String): Boolean = string.endsWith("txt")
  }
  "FileFilter" should "combine filters with &&" in IO.withTemporaryDirectory { dir =>
    val firstFilter = new ExactFilter(dir.getName)
    assert(firstFilter.accept(dir))
    val andFilter = firstFilter && DirectoryFilter
    assert(andFilter.accept(dir))
    assert(andFilter.toString == s"$firstFilter && DirectoryFilter")
  }
  it should "combine filters with &" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "foo.txt")
    val firstFilter = new ExactFilter("foo.txt")
    assert(firstFilter.accept(file))
    val endsWith = endsWithTxt
    val andFilter = firstFilter && new SimpleFilter(endsWith)
    assert(andFilter.accept(file))
    // This is subtle. I'm checking that equality works when the same function instance is used
    // in the SimpleFilter, but a new SimpleFilter is created itself. In the second test case,
    // equality doesn't work because endsWithText creates a new instance of String => Boolean
    // which can't be equal to the endsWithText local variable.
    assert(andFilter == (firstFilter && new SimpleFilter(endsWith)))
    assert(andFilter != (firstFilter && new SimpleFilter(endsWithTxt)))
  }
  it should "combine filters with ||" in IO.withTemporaryDirectory { dir =>
    val firstFilter = new ExactFilter("foo.txt")
    assert(!firstFilter.accept(dir))
    val orFilter = firstFilter || DirectoryFilter
    assert(orFilter.accept(dir))
    assert(orFilter.toString == s"ExactFilter(foo.txt) || DirectoryFilter")
  }
  it should "combine filters with |" in IO.withTemporaryDirectory { dir =>
    val file = new File("bar.txt")
    val firstFilter = new ExactFilter("foo.txt")
    assert(!firstFilter.accept(file))
    val orFilter = firstFilter | new ExactFilter("bar.txt")
    assert(orFilter.accept(file))
  }
  it should "combine filters with --" in IO.withTemporaryDirectory { dir =>
    val firstFilter = new ExactFilter(dir.getName)
    assert(firstFilter.accept(dir.getName))
    val andNotFilter = firstFilter -- DirectoryFilter
    assert(!andNotFilter.accept(dir))
    assert(andNotFilter.toString == s"ExactFilter(${dir.getName}) && !DirectoryFilter")
  }
  it should "combine filters with -" in {
    val file = new File("foo.scala")
    val firstFilter = new ExactFilter("foo.scala")
    assert(firstFilter.accept(file))
    val andNotFilter = firstFilter - new SimpleFilter(_.endsWith("scala"))
    assert(!andNotFilter.accept(file))
  }
  it should "negate filters" in {
    val file = new File("foo.scala")
    val firstFilter = new ExactFilter("foo.scala")
    assert(firstFilter.accept(file))
    val notFilter = -firstFilter
    assert(!notFilter.accept(file))
    assert(notFilter.toString == s"!ExactFilter(foo.scala)")
  }
}
