import sbt._
import Keys._

object Dependencies {
  val scala212 = "2.12.8"
  val scala213 = "2.13.0"

  val scalaCompiler = Def.setting { "org.scala-lang" % "scala-compiler" % scalaVersion.value }

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"
  val scalatest = "org.scalatest" %% "scalatest" % "3.0.8"
  val jna = "net.java.dev.jna" % "jna" % "4.5.0"
  val jnaPlatform = "net.java.dev.jna" % "jna-platform" % "4.5.0"
  val swovalFiles = "com.swoval" % "file-tree-views" % "2.1.2"
  def nightlyVersion: Option[String] = sys.props.get("sbt.build.version")
}
