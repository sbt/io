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

import java.util.regex.Pattern

import org.scalatest.{ FlatSpec, Matchers }

import scala.annotation.tailrec

class NameFilterSpec extends FlatSpec with Matchers {
  "NameFilter" should "have readable toString() method" in {
    AllPassFilter.toString shouldBe "AllPassFilter"
    DirectoryFilter.toString shouldBe "DirectoryFilter"
    ExistsFileFilter.toString shouldBe "ExistsFileFilter"
    HiddenFileFilter.toString shouldBe "HiddenFileFilter"
    NothingFilter.toString shouldBe "NothingFilter"
    new ExactFilter("foo.txt").toString shouldBe s"ExactFilter(foo.txt)"
    new PrefixFilter("foo").toString shouldBe s"PrefixFilter(foo)"
    new SuffixFilter("foo").toString shouldBe s"SuffixFilter(foo)"
    new PatternFilter(Pattern.compile(".*\\.scala")).toString shouldBe s"PatternFilter(.*\\.scala)"
  }
  it should "correctly override equals/hashCode" in {
    val (foo, bar) = ("foo.txt", "bar.txt")
    assert(new ExactFilter(foo) == new ExactFilter(foo))
    assert(new ExactFilter(foo).hashCode == new ExactFilter(foo).hashCode)
    assert(new ExactFilter(foo) != new ExactFilter(bar))
    assert(new ExactFilter(foo).hashCode != new ExactFilter(bar).hashCode)

    val (fooPrefix, barPrefix) = ("foo", "bar")
    assert(new PrefixFilter(fooPrefix) == new PrefixFilter(fooPrefix))
    assert(new PrefixFilter(barPrefix) == new PrefixFilter(barPrefix))
    assert(new PrefixFilter(fooPrefix) != new PrefixFilter(barPrefix))
    assert(new PrefixFilter(fooPrefix).hashCode == fooPrefix.hashCode)
    assert(new PrefixFilter(barPrefix).hashCode == barPrefix.hashCode)
    assert(new PrefixFilter(fooPrefix).hashCode != new PrefixFilter(barPrefix).hashCode)

    val (fooSuffix, barSuffix) = ("foo", "bar")
    assert(new SuffixFilter(fooSuffix) == new SuffixFilter(fooSuffix))
    assert(new SuffixFilter(barSuffix) == new SuffixFilter(barSuffix))
    assert(new SuffixFilter(fooSuffix) != new SuffixFilter(barSuffix))
    assert(new SuffixFilter(fooSuffix).hashCode == fooSuffix.hashCode)
    assert(new SuffixFilter(barSuffix).hashCode == barSuffix.hashCode)
    assert(new SuffixFilter(fooSuffix).hashCode != new SuffixFilter(barSuffix).hashCode)

    val (java, scala) = (Pattern.compile(".*\\.java"), Pattern.compile(".*\\.scala"))
    assert(new PatternFilter(java) == new PatternFilter(java))
    assert(new PatternFilter(java).hashCode == new PatternFilter(java).hashCode)
    assert(new PatternFilter(java) != new PatternFilter(scala))
    assert(new PatternFilter(java).hashCode != new PatternFilter(scala).hashCode)
  }
  it should "correctly identify unequal filters" in {
    val filters = Seq(
      AllPassFilter,
      DirectoryFilter,
      ExistsFileFilter,
      HiddenFileFilter,
      NothingFilter,
      new ExactFilter("foo.txt"),
      new PrefixFilter("foo"),
      new SuffixFilter("bar"),
      new PatternFilter(Pattern.compile(".*\\.scala"))
    )
    @tailrec
    def check(head: FileFilter, tail: Seq[FileFilter]): Unit = {
      tail match {
        case t if t.nonEmpty =>
          t.foreach { filter =>
            assert(head != filter && filter != head)
            assert(head.hashCode != filter.hashCode())
          }
          check(t.head, t.tail)
        case _ => ()
      }
    }
    check(filters.head, filters.tail)
  }
}
