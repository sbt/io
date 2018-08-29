import Dependencies._
import com.typesafe.tools.mima.core._, ProblemFilters._

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
        git.baseVersion := "1.2.0",
        bintrayPackage := "io",
        homepage := Some(url("https://github.com/sbt/io")),
        description := "IO module for sbt",
        scmInfo := Some(ScmInfo(url("https://github.com/sbt/io"), "git@github.com:sbt/io.git")),
        scalafmtOnCompile in Sbt := false,
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
      Vector(scalaCompiler.value % Test, scalaCheck % Test, scalatest.value % Test)
    } ++ Vector(swovalFiles),
    libraryDependencies ++= Seq(jna, jnaPlatform),
    sourceManaged in (Compile, generateContrabands) := baseDirectory.value / "src" / "main" / "contraband-scala",
    initialCommands in console += "\nimport sbt.io._, syntax._",
    mimaPreviousArtifacts := (CrossVersion partialVersion scalaVersion.value match {
      case Some((2, n)) if n >= 13 => Set.empty
      case _                       => 
        Set(
          "1.0.0", "1.0.1", "1.0.2",
          "1.1.0", "1.1.1", "1.1.2", "1.1.3", "1.1.4",
        ) map (version => organization.value %% moduleName.value % version)
    }),
    mimaBinaryIssueFilters ++= Seq(
      // MiMa doesn't treat effectively final members as final
      // WORKAROUND typesafehub/migration-manager#162
      exclude[FinalMethodProblem]("sbt.io.SimpleFilter.accept"),
      exclude[FinalMethodProblem]("sbt.io.SimpleFileFilter.accept"),

      // MiMa doesn't understand private inner classes?
      // method this(sbt.io.PollingWatchService,sbt.io.PollingWatchService#PollingThread,java.nio.file.Watchable,java.util.List)Unit in class sbt.io.PollingWatchService#PollingWatchKey does not have a correspondent in current version
      exclude[DirectMissingMethodProblem]("sbt.io.PollingWatchService#PollingWatchKey.this"),

      // This is a private class
      exclude[DirectMissingMethodProblem]("sbt.io.PollingWatchService#PollingWatchKey.events"),

      // moved JavaMilli to sbt.io
      exclude[MissingClassProblem]("sbt.internal.io.JavaMilli$"),
      exclude[MissingClassProblem]("sbt.internal.io.JavaMilli"),

      // protected[this]
      exclude[DirectMissingMethodProblem]("sbt.io.CopyOptions.copy*"),
    ),
    BuildInfoPlugin.buildInfoDefaultSettings, // avoids BuildInfo generated in Compile scope
    addBuildInfoToConfig(Test),
    buildInfoKeys in Test := Seq[BuildInfoKey](target),
    buildInfoPackage in Test := "sbt.internal.io",
    buildInfoUsePackageAsPath in Test := true,
    scalacOptions := {
      val old = scalacOptions.value
      scalaBinaryVersion.value match {
        case "2.12" => old
        case _ =>
          old filterNot Set(
            "-Xfatal-warnings",
            "-deprecation",
            "-Ywarn-unused",
            "-Ywarn-unused-import",
            "-Yno-adapted-args",
          )
      }
    }
  )

inThisBuild(Seq(
  whitesourceProduct                   := "Lightbend Reactive Platform",
  whitesourceAggregateProjectName      := "sbt-io-master",
  whitesourceAggregateProjectToken     := "460d23766d364910a7c6f8f330f55aea651a3d13b4a94809b79bb6fb63c761ea",
  whitesourceIgnoredScopes             += "scalafmt",
  whitesourceFailOnError               := sys.env.contains("WHITESOURCE_PASSWORD"), // fail if pwd is present
  whitesourceForceCheckAllDependencies := true,
))
