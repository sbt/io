import sbt._
import Keys._

object Dependencies {
  lazy val scala210 = "2.10.5"
  lazy val scala211 = "2.11.6"

  lazy val scalaCompiler = Def.setting { "org.scala-lang" % "scala-compiler" % scalaVersion.value }
}
