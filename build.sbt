import com.typesafe.tools.mima.core._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import Dependencies._

addCommandAlias("fmt", "; scalafmtAll; scalafmtSbt")
addCommandAlias("fmtCheck", "; scalafmtCheckAll; scalafmtSbtCheck")

addCommandAlias("prePR", "; githubWorkflowGenerate ; +fmt; bench/compile; +test")

ThisBuild / baseVersion := "0.3"

ThisBuild / organization := "org.typelevel"
ThisBuild / organizationName := "Typelevel"

ThisBuild / publishGithubUser := "johnynek"
ThisBuild / publishFullName := "P. Oscar Boykin"

ThisBuild / crossScalaVersions := List("3.0.2", "2.11.12", "2.12.15", "2.13.7")

ThisBuild / spiewakCiReleaseSnapshots := true

ThisBuild / spiewakMainBranches := List("main")

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Run(
    List(
      """sbt ++${{ matrix.scala }} fmtCheck \
        |    "++${{ matrix.scala }} test" \
        |    "++${{ matrix.scala }} doc" \
        |    "++${{ matrix.scala }} mimaReportBinaryIssues"""".stripMargin
    )
  )
)

ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    id = "build-docs",
    name = "Build docs",
    scalas = List("2.13.7"),
    steps = List(WorkflowStep.Checkout) ++ WorkflowStep.SetupJava(
      githubWorkflowJavaVersions.value.toList
    ) ++ githubWorkflowGeneratedCacheSteps.value ++ List(WorkflowStep.Sbt(List("docs/mdoc")))
  ),
  WorkflowJob(
    id = "coverage",
    name = "Generate coverage report",
    scalas = List("2.13.7"),
    steps = List(WorkflowStep.Checkout) ++ WorkflowStep.SetupJava(
      githubWorkflowJavaVersions.value.toList
    ) ++ githubWorkflowGeneratedCacheSteps.value ++ List(
      WorkflowStep.Sbt(List("coverage", "test", "coverageAggregate")),
      WorkflowStep.Run(List("bash <(curl -s https://codecov.io/bash)"))
    )
  )
)

ThisBuild / githubWorkflowPublish ++= Seq(
  WorkflowStep.Sbt(List("docs/makeSite")),
  WorkflowStep.Use(
    UseRef.Public("JamesIves", "github-pages-deploy-action", "3.7.1"),
    params = Map(
      "GITHUB_TOKEN" -> "${{ secrets.GITHUB_TOKEN }}",
      "BRANCH" -> "gh-pages",
      "FOLDER" -> "docs/target/site"
    )
  )
)

ThisBuild / homepage := Some(url("https://github.com/typelevel/cats-parse"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/typelevel/cats-parse"),
    "git@github.com:typelevel/cats-parse.git"
  )
)

ThisBuild / licenses := List(("MIT", url("http://opensource.org/licenses/MIT")))

ThisBuild / testFrameworks += new TestFramework("munit.Framework")

lazy val jvmVersionSettings = VersionNumber(sys.props("java.version")) match {
  case v if v.matchesSemVer(SemanticSelector(">1.8")) =>
    Def.settings(
      scalacOptions ++= {
        val isScala211 = CrossVersion.partialVersion(scalaVersion.value).contains((2, 11))
        if (isScala211) List("-target:jvm-1.8") else List("-release", "8")
      },
      // Suppresses problems with Scaladoc @throws links
      Compile / doc / scalacOptions ++= Seq("-no-link-warnings"),
      javacOptions ++= Seq("--release", "8")
    )
  case _ => Def.settings()
}

lazy val root = project
  .in(file("."))
  .aggregate(core.jvm, core.js, bench)
  .enablePlugins(NoPublishPlugin, SonatypeCiReleasePlugin)
  .settings(scalaVersion := "2.13.7")

lazy val docs = project
  .enablePlugins(
    ParadoxSitePlugin,
    ParadoxMaterialThemePlugin,
    MdocPlugin,
    NoPublishPlugin,
    GhpagesPlugin
  )
  .settings(jvmVersionSettings)
  .settings(
    name := "paradox-docs",
    libraryDependencies += jawnAst,
    Compile / paradoxProperties ++= Map(
      "empty" -> "",
      "version" -> version.value
    ),
    githubWorkflowArtifactUpload := false,
    git.remoteRepo := "git@github.com:typelevel/cats-parse.git",
    mdocIn := (Compile / baseDirectory).value / "src",
    Compile / paradox / sourceDirectory := mdocOut.value,
    Compile / paradox := (Compile / paradox).dependsOn(mdoc.toTask("")).value,
    Compile / paradoxMaterialTheme := ParadoxMaterialTheme()
      .withColor("red", "orange")
      .withFont("Ubuntu", "Ubuntu Mono")
      .withCopyright("Copyright (c) 2020 Typelevel")
      .withRepository(uri("https://github.com/typelevel/cats-parse"))
  )
  .dependsOn(coreJVM, bench)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "cats-parse",
    libraryDependencies ++= {
      val isScala211 = CrossVersion.partialVersion(scalaVersion.value).contains((2, 11))
      Seq(
        if (isScala211) cats211.value else cats.value,
        munit.value % Test,
        munitScalacheck.value % Test
      )
    },
    libraryDependencies ++= {
      val isScala2 = CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 2)
      if (isScala2) Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value) else Nil
    },
    scalacOptions ++= {
      val isScala211 = CrossVersion.partialVersion(scalaVersion.value).contains((2, 11))
      // this code seems to trigger a bug in 2.11 pattern analysis
      if (isScala211) List("-Xno-patmat-analysis") else Nil
    },
    mimaPreviousArtifacts := {
      val isScala211 = CrossVersion.partialVersion(scalaVersion.value).contains((2, 11))
      if (isScala211) Set.empty else mimaPreviousArtifacts.value
    }
  )
  .jsSettings(
    crossScalaVersions := (ThisBuild / crossScalaVersions).value.filterNot(_.startsWith("2.11")),
    Global / scalaJSStage := FastOptStage,
    parallelExecution := false,
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    // batch mode decreases the amount of memory needed to compile scala.js code
    scalaJSLinkerConfig := scalaJSLinkerConfig.value
      .withBatchMode(scala.sys.env.get("TRAVIS").isDefined)
      .withModuleKind(ModuleKind.CommonJSModule),
    coverageEnabled := false,
    scalaJSUseMainModuleInitializer := false
  )

lazy val coreJVM = core.jvm.settings(jvmVersionSettings)
lazy val coreJS = core.js

lazy val bench = project
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .settings(
    name := "bench",
    coverageEnabled := false,
    crossScalaVersions := (ThisBuild / crossScalaVersions).value.filter { v =>
      v.startsWith("2.12") || v.startsWith("2.13")
    },
    libraryDependencies ++=
      Seq(
        fastParse,
        parsley,
        jawnAst,
        parboiled,
        attoCore
      ),
    githubWorkflowArtifactUpload := false
  )
  .dependsOn(coreJVM)
