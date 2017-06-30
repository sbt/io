import Dependencies._
// import com.typesafe.tools.mima.core._, ProblemFilters._

def baseVersion: String = "1.0.0-M12"

def commonSettings: Seq[Setting[_]] = Seq(
  scalaVersion := scala212,
  javacOptions in compile ++= Seq("-Xlint", "-Xlint:-serial"),
  crossScalaVersions := Seq(scala210, scala211, scala212, scala213),
  // mimaPreviousArtifacts := Set.empty // Set(organization.value %% moduleName.value % "1.0.0")
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
    skip in publish := true
  )

// Path, IO (formerly FileUtilities), NameFilter and other I/O utility classes
val io = (project in file("io"))
  .enablePlugins(ContrabandPlugin)
  .settings(
    commonSettings,
    name := "IO",
    libraryDependencies ++= Seq(scalaCompiler.value % Test, scalaCheck % Test, scalatest % Test),
    sourceManaged in (Compile, generateContrabands) := baseDirectory.value / "src" / "main" / "contraband-scala",
    initialCommands in console += "\nimport sbt.io._, syntax._"
  )
