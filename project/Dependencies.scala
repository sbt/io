import sbt._
import Keys._

object Dependencies {
  val scala212 = "2.12.18"
  val scala213 = "2.13.12"
  val scala3 = "3.3.1"

  val scalaCompiler = Def.setting {
    val v = if (scalaBinaryVersion.value == "3") scala213 else scalaVersion.value
    "org.scala-lang" % "scala-compiler" % v
  }

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.17.0"
  val scalatest = "org.scalatest" %% "scalatest" % "3.2.16"
  val jnaVersion = "5.13.0"
  val jna = "net.java.dev.jna" % "jna" % jnaVersion
  val jnaPlatform = "net.java.dev.jna" % "jna-platform" % jnaVersion
  val swovalFiles = "com.swoval" % "file-tree-views" % "2.1.11"
}
