import Dependencies._
import com.typesafe.tools.mima.core._, ProblemFilters._


ThisBuild / scalafmtOnCompile := true
ThisBuild / Sbt / scalafmtOnCompile := false
ThisBuild / scalafmtVersion := "1.4.0"

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
        git.baseVersion := "1.3.0",
        bintrayPackage := "io",
        homepage := Some(url("https://github.com/sbt/io")),
        description := "IO module for sbt",
        scmInfo := Some(ScmInfo(url("https://github.com/sbt/io"), "git@github.com:sbt/io.git")),
        scalafmtOnCompile in Sbt := false,
      )),
    commonSettings,
    name := "IO Root",
    skip in publish := true,
    onLoadMessage := {
      """      _     
        |     (_)___ 
        |    / / __ \
        |   / / /_/ /
        |  /_/\____/ 
        |Welcome to the build for sbt/io.
        |""".stripMargin +
          (if (sys.props("java.specification.version") != "1.8")
            s"""!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               |  Java versions is ${sys.props("java.specification.version")}. We recommend 1.8.
               |!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""".stripMargin
          else "")
    }
  )

// Path, IO (formerly FileUtilities), NameFilter and other I/O utility classes
val io = (project in file("io"))
  .enablePlugins(ContrabandPlugin)
  .settings(
    commonSettings,
    name := "IO",
    libraryDependencies ++= {
      Vector(scalaCompiler.value % Test, scalaCheck % Test, scalatest % Test)
    } ++ Vector(swovalFiles),
    libraryDependencies ++= Seq(jna, jnaPlatform),

    Test / fork := true,

    sourceManaged in (Compile, generateContrabands) := baseDirectory.value / "src" / "main" / "contraband-scala",
    initialCommands in console += "\nimport sbt.io._, syntax._",
    mimaPreviousArtifacts := (CrossVersion partialVersion scalaVersion.value match {
      case Some((2, n)) if n >= 13 => Set.empty
      case _                       =>
        Set(
          "1.0.0", "1.0.1", "1.0.2",
          "1.1.0", "1.1.1", "1.1.2", "1.1.3", "1.1.4",
          "1.2.0",
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
      exclude[IncompatibleMethTypeProblem]("sbt.io.PollingWatchService#PollingWatchKey.this"),

      // This is a private class
      exclude[DirectMissingMethodProblem]("sbt.io.PollingWatchService#PollingWatchKey.events"),
      exclude[DirectMissingMethodProblem]("sbt.io.PollingWatchService#PollingWatchKey.offer"),

      // This is a private class
      exclude[DirectMissingMethodProblem]("sbt.io.PollingWatchService#PollingThread.events"),
      exclude[DirectMissingMethodProblem]("sbt.io.PollingWatchService#PollingThread.initDone"),
      exclude[DirectMissingMethodProblem]("sbt.io.PollingWatchService#PollingThread.initDone_="),
      exclude[DirectMissingMethodProblem]("sbt.io.PollingWatchService#PollingThread.keysWithEvents"),
      exclude[DirectMissingMethodProblem]("sbt.io.PollingWatchService#PollingThread.getFileTimes"),

      // moved JavaMilli to sbt.io
      exclude[MissingClassProblem]("sbt.internal.io.JavaMilli$"),
      exclude[MissingClassProblem]("sbt.internal.io.JavaMilli"),

      // These are private classes
      exclude[MissingClassProblem]("sbt.internal.io.FreeBSD64"),
      exclude[MissingClassProblem]("sbt.internal.io.FreeBSD64FileStat"),
      exclude[MissingClassProblem]("sbt.internal.io.FreeBSD64Milli"),
      exclude[MissingClassProblem]("sbt.internal.io.FreeBSD64Milli$"),

      // protected[this]
      exclude[DirectMissingMethodProblem]("sbt.io.CopyOptions.copy*"),

      // private class
      exclude[MissingClassProblem]("sbt.io.Event"),
      exclude[MissingClassProblem]("sbt.io.Event$"),
      exclude[MissingClassProblem]("sbt.io.MacOSXWatchKey"),

      // private internal classes whose functionality has been replaced
      exclude[MissingClassProblem]("sbt.internal.io.EventMonitor$*"),
      exclude[DirectMissingMethodProblem]("sbt.internal.io.EventMonitor.legacy"),
      exclude[DirectMissingMethodProblem]("sbt.internal.io.EventMonitor.applyImpl")
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
