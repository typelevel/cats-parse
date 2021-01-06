import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.3.1")
  lazy val munit = Def.setting("org.scalameta" %%% "munit" % "0.7.20")
  lazy val munitScalacheck = Def.setting("org.scalameta" %%% "munit-scalacheck" % "0.7.20")
  lazy val scalaCheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.14.3")
  lazy val fastParse = "com.lihaoyi" %% "fastparse" % "2.3.0"
  lazy val parsley = "org.http4s" %% "parsley" % "1.5.0-M3"
  lazy val jawnAst = "org.typelevel" %% "jawn-ast" % "1.0.3"
  lazy val parboiled = "org.parboiled" %% "parboiled" % "2.2.1"
  lazy val attoCore = "org.tpolecat" %% "atto-core" % "0.9.0"
}
