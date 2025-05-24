import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.13.0")
  lazy val munit = Def.setting("org.scalameta" %%% "munit" % "1.1.1")
  lazy val munitScalacheck = Def.setting("org.scalameta" %%% "munit-scalacheck" % "1.1.0")
  lazy val fastParse = "com.lihaoyi" %% "fastparse" % "3.1.1"
  lazy val parsley = "org.http4s" %% "parsley" % "1.5.0-M3"
  lazy val jawnAst = Def.setting("org.typelevel" %% "jawn-ast" % "1.6.0")
  lazy val parboiled = "org.parboiled" %% "parboiled" % "2.5.1"
  lazy val attoCore = "org.tpolecat" %% "atto-core" % "0.9.5"
}
