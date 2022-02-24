import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.7.0")
  lazy val cats211 = Def.setting("org.typelevel" %%% "cats-core" % "2.0.0")
  lazy val munit = Def.setting("org.scalameta" %%% "munit" % "0.7.29")
  lazy val munitScalacheck = Def.setting("org.scalameta" %%% "munit-scalacheck" % "0.7.29")
  lazy val fastParse = "com.lihaoyi" %% "fastparse" % "2.3.3"
  lazy val parsley = "org.http4s" %% "parsley" % "1.5.0-M3"
  lazy val jawnAst = "org.typelevel" %% "jawn-ast" % "1.3.2"
  lazy val parboiled = "org.parboiled" %% "parboiled" % "2.4.0"
  lazy val attoCore = "org.tpolecat" %% "atto-core" % "0.9.5"
}
