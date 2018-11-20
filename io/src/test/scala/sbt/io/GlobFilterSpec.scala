package sbt.io
import java.io.File

import org.scalatest.{ FlatSpec, Matchers }

class GlobFilterSpec extends FlatSpec with Matchers {
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
}
