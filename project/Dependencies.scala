import sbt._
import Keys._

object Dependencies {
  lazy val scala210 = "2.10.6"
  lazy val scala211 = "2.11.8"
  lazy val scala212 = "2.12.0-M5"

  lazy val scalaCompiler = Def.setting { "org.scala-lang" % "scala-compiler" % scalaVersion.value }

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.1"
  val scalatest = "org.scalatest" %% "scalatest" % "3.0.0-RC4"
}
