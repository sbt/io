package iobuild

import sbt._
import Keys._

object HouseRulesPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  override def projectSettings: Seq[Def.Setting[?]] = baseSettings

  lazy val baseSettings: Seq[Def.Setting[?]] = Seq(
    scalacOptions ++= Seq("-encoding", "utf8"),
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    scalacOptions += "-language:implicitConversions",
    scalacOptions ++= "-Werror"
      .ifScala(v => {
        sys.props.get("sbt.build.fatal") match {
          case Some(_) => java.lang.Boolean.getBoolean("sbt.build.fatal")
          case _       => v == 12
        }
      })
      .value
      .toList,
  )

  private def scalaPartV = Def setting (CrossVersion partialVersion scalaVersion.value)

  private implicit final class AnyWithIfScala[A](val __x: A) {
    def ifScala(p: Long => Boolean) =
      Def setting (scalaPartV.value collect { case (2, y) if p(y) => __x })
  }
}
