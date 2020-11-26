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

ThisBuild / crossScalaVersions := List("3.0.0-M1", "2.12.12", "2.13.3")

ThisBuild / versionIntroduced := Map(
  "3.0.0-M1" -> "0.1.99"
)

ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)

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
    scalas = List("2.13.3"),
    steps = List(
      WorkflowStep.Checkout,
      WorkflowStep.SetupScala
    ) ++ githubWorkflowGeneratedCacheSteps.value ++ List(WorkflowStep.Sbt(List("docs/mdoc")))
  ),
  WorkflowJob(
    id = "coverage",
    name = "Generate coverage report",
    scalas = List("2.13.3"),
    steps = List(
      WorkflowStep.Checkout,
      WorkflowStep.SetupScala
    ) ++ githubWorkflowGeneratedCacheSteps.value ++ List(
      WorkflowStep.Sbt(List("coverage", "test", "coverageAggregate")),
      WorkflowStep.Run(List("bash <(curl -s https://codecov.io/bash)"))
    )
  )
)

ThisBuild / githubWorkflowEnv ++= Map(
  "SONATYPE_USERNAME" -> s"$${{ secrets.SONATYPE_USERNAME }}",
  "SONATYPE_PASSWORD" -> s"$${{ secrets.SONATYPE_PASSWORD }}",
  "PGP_SECRET" -> s"$${{ secrets.PGP_SECRET }}"
)

ThisBuild / githubWorkflowTargetTags += "v*"

ThisBuild / githubWorkflowPublishPreamble +=
  WorkflowStep.Run(
    List("echo $PGP_SECRET | base64 -d | gpg --import"),
    name = Some("Import signing key")
  )

ThisBuild / githubWorkflowPublish := Seq(WorkflowStep.Sbt(List("release")))

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
  .settings(scalaVersion := "2.13.3")
  .aggregate(core.jvm, core.js, bench)
  .settings(noPublishSettings)

lazy val docs = project
  .enablePlugins(ParadoxPlugin, MdocPlugin)
  .disablePlugins(MimaPlugin)
  .settings(noPublishSettings)
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
    libraryDependencies += cats.value
  )
  .settings(dottyLibrarySettings)
  .settings(dottyJsSettings(ThisBuild / crossScalaVersions))
  .settings(
    libraryDependencies ++=
      Seq(
        munit.value % Test,
        munitScalacheck.value % Test
      )
  )
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
  .jsSettings(crossScalaVersions := crossScalaVersions.value.filter(_.startsWith("2.")))

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .settings(noPublishSettings)
  .settings(
    name := "bench",
    coverageEnabled := false,
    crossScalaVersions := (ThisBuild / crossScalaVersions).value.filter(_.startsWith("2.")),
    libraryDependencies ++= Seq(
      fastParse,
      parsley,
      jawnAst,
      parboiled,
      attoCore
    ),
    githubWorkflowArtifactUpload := false
  )
  .dependsOn(coreJVM)
