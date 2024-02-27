import Dependencies._
import com.typesafe.tools.mima.core._, ProblemFilters._

ThisBuild / version := {
  val old = (ThisBuild / version).value
  (sys.env.get("BUILD_VERSION") orElse sys.props.get("sbt.build.version")) match {
    case Some(v) => v
    case _ =>
      if ((ThisBuild / isSnapshot).value) "1.6.0-SNAPSHOT"
      else old
  }
}
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / organization := "org.scala-sbt"
ThisBuild / homepage := Some(url("https://github.com/sbt/io"))
ThisBuild / description := "IO module for sbt"
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/sbt/io"), "git@github.com:sbt/io.git"))
ThisBuild / licenses := List(("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0")))
ThisBuild / headerLicense := Some(
  HeaderLicense.Custom(
    s"""sbt IO
     |Copyright Scala Center, Lightbend, and Mark Harrah
     |
     |Licensed under Apache License 2.0
     |SPDX-License-Identifier: Apache-2.0
     |
     |See the NOTICE file distributed with this work for
     |additional information regarding copyright ownership.
     |""".stripMargin
  )
)
ThisBuild / scalafmtOnCompile := true
ThisBuild / developers := List(
  Developer("eatkins", "Ethan Atkins", "@eatkins", url("https://www.ethanatkins.com/")),
  Developer("harrah", "Mark Harrah", "@harrah", url("https://github.com/harrah")),
  Developer("eed3si9n", "Eugene Yokota", "@eed3si9n", url("http://eed3si9n.com/")),
  Developer("dwijnand", "Dale Wijnand", "@dwijnand", url("https://github.com/dwijnand")),
)
ThisBuild / turbo := true
ThisBuild / pomIncludeRepository := (_ => false)
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

def commonSettings: Seq[Setting[?]] = Seq(
  scalaVersion := scala212,
  compile / javacOptions ++= Seq("-Xlint", "-Xlint:-serial"),
  crossScalaVersions := Seq(scala212, scala213, scala3),
  headerLicense := (ThisBuild / headerLicense).value,
  scalacOptions ++= {
    val scala2InlineOptions = List(
      "-opt-inline-from:<sources>",
      "-opt:l:inline"
    )
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n == 12 => scala2InlineOptions
      case Some((2, n)) if n == 13 => scala2InlineOptions
      case _                       => Nil
    }
  }
)

lazy val ioRoot = (project in file("."))
  .aggregate(io)
  .settings(
    commonSettings,
    name := "IO Root",
    publish / skip := true,
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
    },
    mimaPreviousArtifacts := Set.empty,
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
    Test / fork := System.getProperty("sbt.test.fork", "false") == "true",
    Test / testForkedParallel := true,
    Compile / generateContrabands / sourceManaged := baseDirectory.value / "src" / "main" / "contraband-scala",
    console / initialCommands += "\nimport sbt.io._, syntax._",
    mimaPreviousArtifacts := (CrossVersion partialVersion scalaVersion.value match {
      case Some((2, n)) if n >= 13 => Set.empty
      case _ =>
        Set(
          "1.0.0",
          "1.0.1",
          "1.0.2",
          "1.1.0",
          "1.1.1",
          "1.1.2",
          "1.1.3",
          "1.1.4",
          "1.2.0",
          "1.3.0",
          "1.4.0",
          "1.5.0",
          "1.6.0",
          "1.7.0",
          "1.8.0",
          "1.9.0",
        ) map (version => organization.value %% moduleName.value % version)
    }),
    mimaBinaryIssueFilters ++= Seq(
      exclude[FinalClassProblem]("sbt.internal.io.MacJNA$TimeBuf"),
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
      exclude[DirectMissingMethodProblem](
        "sbt.io.PollingWatchService#PollingThread.keysWithEvents"
      ),
      exclude[DirectMissingMethodProblem]("sbt.io.PollingWatchService#PollingThread.getFileTimes"),
      // moved JavaMilli to sbt.io
      exclude[MissingClassProblem]("sbt.internal.io.JavaMilli$"),
      exclude[MissingClassProblem]("sbt.internal.io.JavaMilli"),
      // These are private classes
      exclude[MissingClassProblem]("sbt.internal.io.*"),
      // Replaced non-standard __xstat64() with conformant stat() calls
      exclude[DirectMissingMethodProblem]("sbt.internal.io.Linux32.*"),
      exclude[ReversedMissingMethodProblem]("sbt.internal.io.Linux32.*"),
      exclude[DirectMissingMethodProblem]("sbt.internal.io.Linux64.*"),
      exclude[ReversedMissingMethodProblem]("sbt.internal.io.Linux64.*"),
      // protected[this]
      exclude[DirectMissingMethodProblem]("sbt.io.CopyOptions.copy*"),
      // private class
      exclude[MissingClassProblem]("sbt.io.Event"),
      exclude[MissingClassProblem]("sbt.io.Event$"),
      exclude[MissingClassProblem]("sbt.io.MacOSXWatchKey"),
      exclude[MissingClassProblem]("sbt.io.PollingWatchEvent"),
      exclude[MissingClassProblem]("sbt.io.PollingWatchService$PollingWatchKey"),
      exclude[MissingClassProblem]("sbt.io.PollingWatchService$PollingThread"),
      exclude[MissingClassProblem]("sbt.io.PollingWatchService$Overflow$"),
      // private internal classes whose functionality has been replaced
      exclude[MissingClassProblem]("sbt.internal.io.EventMonitor*"),
      exclude[DirectMissingMethodProblem]("sbt.internal.io.EventMonitor.legacy"),
      exclude[DirectMissingMethodProblem]("sbt.internal.io.EventMonitor.applyImpl"),
      // private classes that have been removed
      exclude[MissingClassProblem]("sbt.internal.io.Alternatives$"),
      exclude[MissingClassProblem]("sbt.internal.io.Alternatives"),
      exclude[DirectMissingMethodProblem]("sbt.io.NothingFilter.unary_-"),
      exclude[DirectMissingMethodProblem]("sbt.io.AllPassFilter.unary_-"),
      exclude[IncompatibleSignatureProblem]("sbt.io.PollingWatchService.pollEvents"),
      exclude[IncompatibleSignatureProblem]("sbt.io.WatchService#WatchServiceAdapter.pollEvents"),
      exclude[IncompatibleSignatureProblem]("sbt.io.WatchService.pollEvents"),
    ),
    BuildInfoPlugin.buildInfoDefaultSettings, // avoids BuildInfo generated in Compile scope
    addBuildInfoToConfig(Test),
    Test / buildInfoKeys := Seq[BuildInfoKey](target),
    Test / buildInfoPackage := "sbt.internal.io",
    Test / buildInfoUsePackageAsPath := true,
  )
