import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.2.0")
  lazy val munit = Def.setting("org.scalameta" %%% "munit" % "0.7.18")
  lazy val munitScalacheck = Def.setting("org.scalameta" %%% "munit-scalacheck" % "0.7.18")
  lazy val scalaCheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.14.3")
}
