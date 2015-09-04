import sbt._
import Keys._

object Dependencies {
  lazy val scala210 = "2.10.5"
  lazy val scala211 = "2.11.7"

  lazy val scalaCompiler = Def.setting { "org.scala-lang" % "scala-compiler" % scalaVersion.value }

  val scalaCheck           = "org.scalacheck" %% "scalacheck" % "1.12.4"
  val scalatest            = "org.scalatest" %% "scalatest" % "2.2.4"
}
