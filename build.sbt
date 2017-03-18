import Dependencies._
import com.typesafe.tools.mima.core._, ProblemFilters._

def baseVersion: String = "1.0.0-M8"

def commonSettings: Seq[Setting[_]] = Seq(
  scalaVersion := scala211,
  javacOptions in compile ++= Seq("-Xlint", "-Xlint:-serial"),
  scalacOptions  -= "-Yinline-warnings",
  scalacOptions  += "-Xfatal-warnings",
  scalacOptions ++= ifScala211Plus("-Ywarn-unused").value.toList,
  scalacOptions ++= ifScala211Plus("-Ywarn-unused-import").value.toList,
  crossScalaVersions := Seq(scala210, scala211, scala212),
  mimaPreviousArtifacts := Set.empty // Set(organization.value %% moduleName.value % "1.0.0"),
) ++ relaxNon212

def relaxNon212: Seq[Setting[_]] = Seq(
  scalacOptions := {
      val old = scalacOptions.value
      scalaBinaryVersion.value match {
        case "2.12" => old
        case _      => old filterNot Set("-Xfatal-warnings", "-deprecation", "-Ywarn-unused", "-Ywarn-unused-import")
      }
    }
)

lazy val ioRoot = (project in file(".")).
  aggregate(io).
  settings(
    inThisBuild(Seq(
      git.baseVersion := baseVersion,
      bintrayPackage := "io",
      homepage := Some(url("https://github.com/sbt/io")),
      description := "IO module for sbt",
      scmInfo := Some(ScmInfo(url("https://github.com/sbt/io"), "git@github.com:sbt/io.git"))
    )),
    commonSettings,
    name := "IO Root",
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

// Path, IO (formerly FileUtilities), NameFilter and other I/O utility classes
lazy val io = (project in file("io")).
  settings(
    commonSettings,
    name := "IO",
    libraryDependencies ++= Seq(scalaCompiler.value % Test, scalaCheck % Test, scalatest % Test)
  )

def ifScala211Plus[T](x: T) = Def.setting(
  PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){ case Some((2, v)) if v >= 11 => x }
)
