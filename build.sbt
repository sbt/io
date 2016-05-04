import Dependencies._
import com.typesafe.tools.mima.core._, ProblemFilters._

def baseVersion: String = "1.0.0-M5"

def commonSettings: Seq[Setting[_]] = Seq(
  scalaVersion := scala211,
  javacOptions in compile ++= Seq("-Xlint", "-Xlint:-serial"),
  scalacOptions ++= Seq("-encoding", "utf8"),
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint"),
  scalacOptions  += "-language:higherKinds",
  scalacOptions  += "-language:implicitConversions",
  scalacOptions  += "-Xfuture",
  scalacOptions  -= "-Yinline-warnings",
  scalacOptions  += "-Xfatal-warnings",
  scalacOptions  += "-Yno-adapted-args",
  scalacOptions  += "-Ywarn-dead-code",
  scalacOptions  += "-Ywarn-numeric-widen",
  scalacOptions  += "-Ywarn-value-discard",
  incOptions := incOptions.value.withNameHashing(true),
  crossScalaVersions := Seq(scala210, scala211), // scala212
  previousArtifact := None, // Some(organization.value %% moduleName.value % "1.0.0"),
  publishArtifact in Compile := true,
  publishArtifact in Test := true
)

lazy val root = (project in file(".")).
  aggregate(io).
  settings(
    inThisBuild(Seq(
      git.baseVersion := baseVersion,
      bintrayPackage := "io",
      homepage := Some(url("https://github.com/sbt/io")),
      description := "IO module for sbt",
      scmInfo := Some(ScmInfo(url("https://github.com/sbt/io"), "git@github.com:sbt/io.git"))
    )),
    name := "IO Root",
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

// Path, IO (formerly FileUtilities), NameFilter and other I/O utility classes
lazy val io = (project in file("io")).
  // dependsOn(controlProj).
  settings(
    commonSettings,
    // testedBaseSettings,
    // Util.crossBuild,
    name := "IO",
    libraryDependencies ++= Seq(scalaCompiler.value % Test, scalaCheck % Test, scalatest % Test),
    binaryIssueFilters ++= Seq(
    )
  )
