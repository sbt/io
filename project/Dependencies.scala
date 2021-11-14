import sbt._
import Keys._

object Dependencies {
  val scala212 = "2.12.15"
  val scala213 = "2.13.6"

  val scalaCompiler = Def.setting {
    val v = if (scalaBinaryVersion.value == "3") scala213 else scalaVersion.value
    "org.scala-lang" % "scala-compiler" % v
  }

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.15.4"
  val scalatest = "org.scalatest" %% "scalatest" % "3.2.10"
  val jna = "net.java.dev.jna" % "jna" % "5.8.0"
  val jnaPlatform = "net.java.dev.jna" % "jna-platform" % "5.8.0"
  val swovalFiles = "com.swoval" % "file-tree-views" % "2.1.7"
}
