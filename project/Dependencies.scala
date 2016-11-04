import sbt._
import Keys._

object Dependencies {
  val scala210 = "2.10.6"
  val scala211 = "2.11.8"
  val scala212 = "2.12.0"

  val scalaCompiler = Def.setting { "org.scala-lang" % "scala-compiler" % scalaVersion.value }

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4"
  val scalatest = "org.scalatest" %% "scalatest" % "3.0.0"
}
