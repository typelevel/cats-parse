import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.9.0")
  lazy val cats211 = Def.setting("org.typelevel" %%% "cats-core" % "2.0.0")
  lazy val munit = Def.setting("org.scalameta" %%% "munit" % "1.0.0-M6")
  lazy val munitScalacheck = Def.setting("org.scalameta" %%% "munit-scalacheck" % "1.0.0-M6")
  lazy val fastParse = "com.lihaoyi" %% "fastparse" % "3.0.1"
  lazy val parsley = "org.http4s" %% "parsley" % "1.5.0-M3"
  lazy val jawnAst = Def.setting("org.typelevel" %% "jawn-ast" % "1.4.0")
  lazy val jawnAst211 = Def.setting("org.typelevel" %% "jawn-ast" % "1.0.0-RC2")
  lazy val parboiled = "org.parboiled" %% "parboiled" % "2.4.1"
  lazy val attoCore = "org.tpolecat" %% "atto-core" % "0.9.5"
}
