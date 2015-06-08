import Dependencies._

def commonSettings: Seq[Setting[_]] = Seq(
  scalaVersion := "2.10.5",
  javacOptions in compile ++= Seq("-Xlint", "-Xlint:-serial"),
  scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-deprecation", "-Xlint"),
  incOptions := incOptions.value.withNameHashing(true),
  crossScalaVersions := Seq(scala210, scala211),
  bintrayPackage := (bintrayPackage in ThisBuild).value,
  bintrayRepository := (bintrayRepository in ThisBuild).value
)

lazy val root = (project in file(".")).
  aggregate(io).
  settings(
    inThisBuild(Seq(
      organization := "org.scala-sbt",
      version := "0.1.0-SNAPSHOT",
      homepage := Some(url("https://github.com/sbt/io")),
      description := "IO module for sbt",
      licenses := List("BSD New" -> url("https://github.com/sbt/sbt/blob/0.13/LICENSE")),
      scmInfo := Some(ScmInfo(url("https://github.com/sbt/io"), "git@github.com:sbt/io.git")),
      developers := List(
        Developer("harrah", "Mark Harrah", "@harrah", url("https://github.com/harrah")),
        Developer("eed3si9n", "Eugene Yokota", "@eed3si9n", url("https://github.com/eed3si9n")),
        Developer("jsuereth", "Josh Suereth", "@jsuereth", url("https://github.com/jsuereth"))
      ),
      bintrayReleaseOnPublish := false,
      bintrayOrganization := Some("sbt"),
      bintrayRepository := "maven-releases",
      bintrayPackage := "io"
    )),
    publish := ()
  )

// Path, IO (formerly FileUtilities), NameFilter and other I/O utility classes
lazy val io = (project in file("io")).
  // dependsOn(controlProj).
  settings(
    commonSettings,
    // testedBaseSettings,
    // Util.crossBuild,
    name := "IO",
    libraryDependencies ++= Seq(scalaCompiler.value % Test, scalaCheck % Test, specs2 % Test),
    crossScalaVersions := Seq(scala210, scala211)
  )
