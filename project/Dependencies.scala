import sbt._
import Keys._

object Dependencies {
  val scala212 = "2.12.16"
  val scala213 = "2.13.8"
  val scala3 = "3.1.3"

  val scalaCompiler = Def.setting {
    val v = if (scalaBinaryVersion.value == "3") scala213 else scalaVersion.value
    "org.scala-lang" % "scala-compiler" % v
  }

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.15.4"
  val scalatest = "org.scalatest" %% "scalatest" % "3.2.10"
  val jnaVersion = "5.12.0"
  val jna = "net.java.dev.jna" % "jna" % jnaVersion
  val jnaPlatform = "net.java.dev.jna" % "jna-platform" % jnaVersion
  val swovalFiles = "com.swoval" % "file-tree-views" % "2.1.10"
}
