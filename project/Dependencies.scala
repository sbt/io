import sbt._
import Keys._

object Dependencies {
  val scala3 = "3.7.4"

  val scalaCompiler = Def.setting {
    val v = scalaVersion.value
    "org.scala-lang" %% "scala3-compiler" % v
  }

  val scalaVerify = "com.eed3si9n.verify" %% "verify" % "1.0.0"
  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.19.0"
  val scalatest = "org.scalatest" %% "scalatest" % "3.2.19"
  val swovalFiles = "com.swoval" % "file-tree-views" % "2.1.12"
}
