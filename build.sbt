import Dependencies._

ThisBuild / tlBaseVersion := "0.3"
ThisBuild / startYear := Some(2021)
ThisBuild / developers += tlGitHubDev("johnynek", "P. Oscar Boykin")

ThisBuild / crossScalaVersions := List("3.0.2", "2.11.12", "2.12.16", "2.13.8")
ThisBuild / tlVersionIntroduced := Map("3" -> "0.3.4")
ThisBuild / tlSkipIrrelevantScalas := true

ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    id = "coverage",
    name = "Generate coverage report",
    scalas = List("2.13.8"),
    steps = List(WorkflowStep.Checkout) ++ WorkflowStep.SetupJava(
      githubWorkflowJavaVersions.value.toList
    ) ++ githubWorkflowGeneratedCacheSteps.value ++ List(
      WorkflowStep.Sbt(List("coverage", "test", "coverageAggregate")),
      WorkflowStep.Run(List("bash <(curl -s https://codecov.io/bash)"))
    )
  )
)

ThisBuild / licenses := List(License.MIT)

lazy val root = tlCrossRootProject.aggregate(core, bench)

lazy val docs =
  project.in(file("site")).enablePlugins(TypelevelSitePlugin).dependsOn(core.jvm, bench)

lazy val isScala211 = Def.setting {
  scalaBinaryVersion.value == "2.11"
}

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "cats-parse",
    tlFatalWarningsInCi := !tlIsScala3.value,
    libraryDependencies ++= {
      Seq(
        if (isScala211.value) cats211.value else cats.value,
        munit.value % Test,
        munitScalacheck.value % Test
      )
    },
    libraryDependencies ++= {
      if (tlIsScala3.value) Nil else Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
    },
    scalacOptions ++= {
      // this code seems to trigger a bug in 2.11 pattern analysis
      if (isScala211.value) List("-Xno-patmat-analysis") else Nil
    },
    mimaPreviousArtifacts := {
      if (isScala211.value) Set.empty else mimaPreviousArtifacts.value
    },
    mimaBinaryIssueFilters ++= MimaExclusionRules.parserImpl ++ MimaExclusionRules.bitSetUtil
  )
  .jsSettings(
    crossScalaVersions := (ThisBuild / crossScalaVersions).value.filterNot(_.startsWith("2.11")),
    coverageEnabled := false
  )

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
  .dependsOn(core.jvm)
