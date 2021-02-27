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

ThisBuild / crossScalaVersions := List("3.0.0-M3", "3.0.0-RC1", "2.13.5", "2.13.4")

ThisBuild / versionIntroduced := Map(
  "3.0.0-M2" -> "0.1.99",
  "3.0.0-M3" -> "0.1.99"
)

ThisBuild / spiewakCiReleaseSnapshots := true

ThisBuild / spiewakMainBranches := List("main")

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Run(
    List(
      """sbt ++${{ matrix.scala }} fmtCheck \
        |    "++${{ matrix.scala }} test" \
        |    "++${{ matrix.scala }} mimaReportBinaryIssues"""".stripMargin
    )
  )
)

ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    id = "build-docs",
    name = "Build docs",
    scalas = List("2.13.4"),
    steps = List(
      WorkflowStep.Checkout,
      WorkflowStep.SetupScala
    ) ++ githubWorkflowGeneratedCacheSteps.value ++ List(WorkflowStep.Sbt(List("docs/mdoc")))
  ),
  WorkflowJob(
    id = "coverage",
    name = "Generate coverage report",
    scalas = List("2.13.4"),
    steps = List(
      WorkflowStep.Checkout,
      WorkflowStep.SetupScala
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

lazy val root = project
  .in(file("."))
  .aggregate(core.jvm, core.js, bench)
  .enablePlugins(NoPublishPlugin, SonatypeCiReleasePlugin)
  .settings(scalaVersion := "2.13.4")

lazy val docs = project
  .enablePlugins(
    ParadoxSitePlugin,
    ParadoxMaterialThemePlugin,
    MdocPlugin,
    NoPublishPlugin,
    GhpagesPlugin
  )
  .settings(
    name := "paradox-docs",
    libraryDependencies += jawnAst,
    paradoxProperties in Compile ++= Map(
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
    libraryDependencies ++=
      Seq(
        cats.value,
        munit.value % Test,
        munitScalacheck.value % Test
      )
  )
  .settings(dottyJsSettings(ThisBuild / crossScalaVersions))
  .jsSettings(
    scalaJSStage in Global := FastOptStage,
    parallelExecution := false,
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    // batch mode decreases the amount of memory needed to compile scala.js code
    scalaJSLinkerConfig := scalaJSLinkerConfig.value
      .withBatchMode(scala.sys.env.get("TRAVIS").isDefined)
      .withModuleKind(ModuleKind.CommonJSModule),
    coverageEnabled := false,
    scalaJSUseMainModuleInitializer := false
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val bench = project
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .settings(
    name := "bench",
    coverageEnabled := false,
    crossScalaVersions := (ThisBuild / crossScalaVersions).value.filter(_.startsWith("2.")),
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
