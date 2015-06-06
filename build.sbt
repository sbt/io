import Dependencies._

// ThisBuild settings take lower precedence,
// but can be shared across the multi projects.
def buildLevelSettings: Seq[Setting[_]] = Seq(
  organization in ThisBuild := "org.scala-sbt",
  version in ThisBuild := "0.1.0-SNAPSHOT"
  // bintrayOrganization in ThisBuild := Some("sbt"),
  // bintrayRepository in ThisBuild := s"ivy-${(publishStatus in ThisBuild).value}",
  // bintrayPackage in ThisBuild := "sbt",
  // bintrayReleaseOnPublish in ThisBuild := false
)

def commonSettings: Seq[Setting[_]] = Seq(
  scalaVersion := "2.10.5",
  javacOptions in compile ++= Seq("-Xlint", "-Xlint:-serial"),
  scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-deprecation", "-Xlint"),
  incOptions := incOptions.value.withNameHashing(true)
  // crossScalaVersions := Seq(scala210),
  // bintrayPackage := (bintrayPackage in ThisBuild).value,
  // bintrayRepository := (bintrayRepository in ThisBuild).value
)

lazy val root = (project in file(".")).
  aggregate(io).
  settings(
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
