package iobuild

import sbt._
import sbt.util.CacheImplicits.given
import Keys._

object HouseRulesPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  override def projectSettings: Seq[Def.Setting[?]] = baseSettings

  lazy val baseSettings: Seq[Def.Setting[?]] = Seq(
    scalacOptions ++= Seq("-encoding", "utf8"),
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    scalacOptions += "-language:implicitConversions",
    scalacOptions ++= Seq(
      "-Wconf:msg=Compiler synthesis of Manifest and OptManifest is deprecated:silent",
      "-Wconf:msg=type Traversable in package scala:silent",
    ),
    scalacOptions ++= {
      sys.props.get("sbt.build.fatal") match {
        case Some("false") =>
          Nil
        case _ =>
          Seq("-Werror")
      }
    }
  )
}
