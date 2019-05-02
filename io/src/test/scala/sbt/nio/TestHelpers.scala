package sbt.nio

import java.nio.file.FileSystems

import scala.util.Properties
import scala.collection.JavaConverters._

object TestHelpers {
  val root = FileSystems.getDefault.getRootDirectories.asScala.head
  val basePath = root.resolve("foo").resolve("bar")
  implicit class StringPathOps(val sc: StringContext) extends AnyVal {
    def p(args: Any*): String = {
      val raw = sc.parts
        .zipAll(args, "", "")
        .map { case (a, p) => s"$p$a" }
        .mkString("")
      if (Properties.isWin) raw.replaceAll("/", "\\\\") else raw
    }
  }
}
