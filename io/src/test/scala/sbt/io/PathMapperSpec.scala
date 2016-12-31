package sbt.io

import org.scalatest._

import Path._, sbt.io.syntax._

class PathMapperSpec extends FlatSpec with Matchers {
  "PathMapper" should "create copy resource mappings correctly" in {
    val base = file("/")

    val files = Seq(base / "src" / "main" / "resources" / "scalac-plugin.xml")
    val dirs = Seq(
      base / "src" / "main" / "resources",
      base / "target" / "scala-2.11" / "resource_managed" / "main"
    )
    val target = base / "target" / "scala-2.11" / "classes"

    val mappings = (files --- dirs) pair (rebase(dirs, target) | flat(target))

    mappings shouldBe Seq(
      base / "src" / "main" / "resources" / "scalac-plugin.xml" ->
        base / "target" / "scala-2.11" / "classes" / "scalac-plugin.xml"
    )
  }
}
