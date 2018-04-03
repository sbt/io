import sbt._
import Keys._

object Dependencies {
  val scala210 = "2.10.7"
  val scala211 = "2.11.12"
  val scala212 = "2.12.4"
  val scala213 = "2.13.0-M3"

  val scalaCompiler = Def.setting { "org.scala-lang" % "scala-compiler" % scalaVersion.value }

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4"
  val scalatest = "org.scalatest" %% "scalatest" % "3.0.3"
  val jna = "net.java.dev.jna" % "jna" % "4.5.0"
  val jnaPlatform = "net.java.dev.jna" % "jna-platform" % "4.5.0"
  val appleFileEvents = "com.swoval" % "apple-file-events" % "1.3.0"
}
