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
    new PatternFilter(Pattern.compile(".*\\.scala")).toString shouldBe s"PatternFilter(.*\\.scala)"
  }
  it should "correctly override equals/hashCode" in {
    val (foo, bar) = ("foo.txt", "bar.txt")
    assert(new ExactFilter(foo) == new ExactFilter(foo))
    assert(new ExactFilter(foo).hashCode == new ExactFilter(foo).hashCode)
    assert(new ExactFilter(foo) != new ExactFilter(bar))
    assert(new ExactFilter(foo).hashCode != new ExactFilter(bar).hashCode)

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
