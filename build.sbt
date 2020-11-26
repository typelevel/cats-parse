import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import Dependencies._

addCommandAlias("fmt", "; scalafmtAll; scalafmtSbt")
addCommandAlias("fmtCheck", "; scalafmtCheckAll; scalafmtSbtCheck")

addCommandAlias("prePR", "; githubWorkflowGenerate ; +fmt; bench/compile; +test")

ThisBuild / baseVersion := "0.1"

ThisBuild / organization := "org.typelevel"
ThisBuild / organizationName := "Typelevel"

ThisBuild / publishGithubUser := "johnynek"
ThisBuild / publishFullName := "P. Oscar Boykin"

ThisBuild / crossScalaVersions := List("3.0.0-M2", "3.0.0-M1", "2.12.12", "2.13.4")

ThisBuild / versionIntroduced := Map(
  "3.0.0-M1" -> "0.1.99",
  "3.0.0-M2" -> "0.1.99"
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
  .enablePlugins(NoPublishPlugin, SonatypeCiRelease)
  .settings(scalaVersion := "2.13.4")

lazy val docs = project
  .enablePlugins(ParadoxPlugin, MdocPlugin, NoPublishPlugin)
  .settings(
    name := "paradox-docs",
    libraryDependencies += jawnAst,
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    paradoxProperties in Compile ++= Map(
      "empty" -> "",
      "version" -> version.value
    ),
    githubWorkflowArtifactUpload := false,
    mdocIn := (paradox / sourceDirectory).value,
    paradox / sourceManaged := mdocOut.value,
    Compile / paradox := (Compile / paradox).dependsOn(mdoc.toTask("")).value
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
