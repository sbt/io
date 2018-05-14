package sbt.internal.io

import java.io.File
import java.nio.file.Paths

import org.scalatest.{ FlatSpec, Matchers }
import sbt.io.{ AllPassFilter, NothingFilter, SimpleFileFilter }

class SourceSpec extends FlatSpec with Matchers {
  it should "accept recursive paths" in {
    val source = new Source(new File("/foo"), AllPassFilter, NothingFilter, true)
    source.accept(Paths.get("/foo/bar/baz")) shouldBe true
  }
  it should "reject subdirectories without recursive flag" in {
    val source = new Source(new File("/foo"), AllPassFilter, NothingFilter, false)
    source.accept(Paths.get("/foo/bar/baz")) shouldBe false
  }
  it should "apply include filter" in {
    val source = new Source(new File("/foo"),
                            new SimpleFileFilter(_.toString.endsWith(".scala")),
                            NothingFilter,
                            true)
    source.accept(Paths.get("/foo/bar/baz.scala")) shouldBe true
    source.accept(Paths.get("/foo/bar/baz.java")) shouldBe false
  }
  it should "apply exclude filter" in {
    val source = new Source(new File("/foo"),
                            new SimpleFileFilter(_.toString.endsWith(".scala")),
                            new SimpleFileFilter(_ == sbt.io.syntax.file("/foo/bar/buzz.scala")),
                            true)
    source.accept(Paths.get("/foo/bar/baz.scala")) shouldBe true
    source.accept(Paths.get("/foo/bar/buzz.scala")) shouldBe false
  }
}
