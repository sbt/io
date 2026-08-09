import Dependencies._
import com.typesafe.tools.mima.core._, ProblemFilters._

ThisBuild / version := {
  val old = (ThisBuild / version).value
  (sys.env.get("BUILD_VERSION") orElse sys.props.get("sbt.build.version")) match {
    case Some(v) => v
    case _       =>
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
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

def commonSettings: Seq[Setting[?]] = Seq(
  scalaVersion := scala212,
  compile / javacOptions ++= Seq("-Xlint", "-Xlint:-serial"),
  crossScalaVersions := Seq(scala212, scala213, scala3),
  headerLicense := (ThisBuild / headerLicense).value,
)

lazy val ioRoot = (project in file("."))
  .aggregate(io, ioBenchmarks)
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
      Vector(scalaCompiler.value % Test, scalaVerify % Test, scalaCheck % Test, scalatest % Test)
    } ++ Vector(swovalFiles),
    testFrameworks += new TestFramework("verify.runner.Framework"),
    Test / fork := System.getProperty("sbt.test.fork", "false") == "true",
    Test / testForkedParallel := true,
    Compile / generateContrabands / sourceManaged := baseDirectory.value / "src" / "main" / "contraband-scala",
    console / initialCommands += "\nimport sbt.io._, syntax._",
    mimaPreviousArtifacts := (CrossVersion partialVersion scalaVersion.value match {
      case Some((2, n)) if n >= 13 => Set.empty
      case _                       =>
        Set(
          "1.12.0",
        ) map (version => organization.value %% moduleName.value % version)
    }),
    mimaBinaryIssueFilters ++= Seq(
    ),
    BuildInfoPlugin.buildInfoDefaultSettings, // avoids BuildInfo generated in Compile scope
    addBuildInfoToConfig(Test),
    Test / buildInfoKeys := Seq[BuildInfoKey](target),
    Test / buildInfoPackage := "sbt.internal.io",
    Test / buildInfoUsePackageAsPath := true,
  )

// Benchmarks for the archive writers. Aggregated so it keeps compiling, but never run by `test`:
// JMH runs on demand via `ioBenchmarks/Jmh/run`.
lazy val ioBenchmarks = (project in file("benchmarks"))
  .dependsOn(io)
  .enablePlugins(JmhPlugin)
  .settings(
    commonSettings,
    name := "IO Benchmarks",
    publish / skip := true,
    // Must cross-build with `io`, even though benchmarks only ever run on one version: `++`
    // filters projects by crossScalaVersions, so pinning this to 2.12 would leave the benchmark
    // on 2.12 while `io` moves, turning dependsOn(io) into an external resolve that cannot exist.
    mimaPreviousArtifacts := Set.empty,
  )
