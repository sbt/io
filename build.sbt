import Dependencies._
import com.typesafe.tools.mima.core._, ProblemFilters._

def baseVersion: String = "1.1.0"

def commonSettings: Seq[Setting[_]] = Seq(
  scalaVersion := scala212,
  javacOptions in compile ++= Seq("-Xlint", "-Xlint:-serial"),
  crossScalaVersions := Seq(scala210, scala211, scala212, scala213),
)

lazy val ioRoot = (project in file("."))
  .aggregate(io)
  .settings(
    inThisBuild(
      Seq(
        git.baseVersion := baseVersion,
        bintrayPackage := "io",
        homepage := Some(url("https://github.com/sbt/io")),
        description := "IO module for sbt",
        scmInfo := Some(ScmInfo(url("https://github.com/sbt/io"), "git@github.com:sbt/io.git")),
        scalafmtOnCompile := true,
        scalafmtVersion := "1.2.0",
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
    libraryDependencies ++= {
      if (scalaVersion.value startsWith "2.13.") Vector()
      else Vector(scalaCompiler.value % Test, scalaCheck % Test, scalatest % Test)
    },
    sourceManaged in (Compile, generateContrabands) := baseDirectory.value / "src" / "main" / "contraband-scala",
    initialCommands in console += "\nimport sbt.io._, syntax._",
    mimaPreviousArtifacts := Set(organization.value %% moduleName.value % "1.0.0"),
    mimaBinaryIssueFilters ++= Seq(
      // MiMa doesn't treat effectively final members as final
      // WORKAROUND typesafehub/migration-manager#162
      exclude[FinalMethodProblem]("sbt.io.SimpleFilter.accept"),
      exclude[FinalMethodProblem]("sbt.io.SimpleFileFilter.accept"),
    ),
  )
