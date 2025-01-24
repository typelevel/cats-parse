import com.typesafe.tools.mima.core._
import Dependencies._
val scala212 = "2.12.20"
val scala213 = "2.13.15"
val scala3 = "3.3.4"

GlobalScope / tlCommandAliases ++= Map(
  "fmt" -> List("scalafmtAll", "scalafmtSbt"),
  "fmtCheck" -> List("scalafmtCheckAll", "scalafmtSbtCheck"),
  "prePR" -> List("githubWorkflowGenerate", "+fmt", "bench/compile", "+test")
)

ThisBuild / tlBaseVersion := "1.1"
// continue enforcing bincompat with 0.3.x series
ThisBuild / tlMimaPreviousVersions ++= (0 to 10).map(x => s"0.3.$x").toSet

ThisBuild / startYear := Some(2021)
ThisBuild / developers += tlGitHubDev("johnynek", "P. Oscar Boykin")

ThisBuild / crossScalaVersions := List(scala212, scala213, scala3)
ThisBuild / scalaVersion := scala213

ThisBuild / tlVersionIntroduced := Map("3" -> "0.3.4")

ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    id = "coverage",
    name = "Generate coverage report",
    scalas = Nil,
    sbtStepPreamble = Nil,
    steps = List(WorkflowStep.Checkout) ++ WorkflowStep.SetupJava(
      githubWorkflowJavaVersions.value.toList
    ) ++ githubWorkflowGeneratedCacheSteps.value ++ List(
      WorkflowStep.Sbt(List("coverage", "rootJVM/test", "coverageAggregate")),
      WorkflowStep.Use(
        UseRef.Public(
          "codecov",
          "codecov-action",
          "v3"
        )
      )
    )
  )
)

ThisBuild / licenses := List(License.MIT)

lazy val root = tlCrossRootProject.aggregate(core, bench)

lazy val docs =
  project.in(file("site")).enablePlugins(TypelevelSitePlugin).dependsOn(core.jvm, bench)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "cats-parse",
    libraryDependencies ++=
      Seq(
        cats.value,
        munit.value % Test,
        munitScalacheck.value % Test
      ),
    libraryDependencies ++= {
      if (tlIsScala3.value) Nil else Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
    },
    mimaBinaryIssueFilters ++= {
      /*
       * It is okay to filter anything in Impl or RadixNode which are private
       */
      if (tlIsScala3.value)
        List(
          ProblemFilters.exclude[DirectMissingMethodProblem]("cats.parse.Parser#Error.fromProduct"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("cats.parse.Parser#Error.unapply"),
          ProblemFilters.exclude[MissingTypesProblem]("cats.parse.Parser$Error$"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("cats.parse.Parser#Error.unapply"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("cats.parse.Parser#Error.fromProduct")
        )
      else Nil
    } ++ MimaExclusionRules.parserImpl ++ MimaExclusionRules.bitSetUtil
  )
  .jvmSettings(
    // We test against jawn on JVM for some json parsers
    libraryDependencies += jawnAst.value % Test
  )
  .jsSettings(
    coverageEnabled := false
  )
  .nativeSettings(
    // cats-parse 1.0.1 switches to Scala Native 0.5, reset tlVersionIntroduced
    tlVersionIntroduced := List("2.12", "2.13", "3").map(_ -> "1.0.1").toMap,
    coverageEnabled := false
  )

lazy val bench = project
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .settings(
    name := "bench",
    coverageEnabled := false,
    scalacOptions += "-Wconf:cat=unused-nowarn:s",
    Compile / unmanagedSources := {
      if (Set("2.12", "2.13").contains(scalaBinaryVersion.value)) {
        (Compile / unmanagedSources).value
      } else Nil
    },
    libraryDependencies ++= {
      if (Set("2.12", "2.13").contains(scalaBinaryVersion.value))
        Seq(
          fastParse,
          parsley,
          jawnAst.value,
          parboiled,
          attoCore
        )
      else Nil
    }
  )
  .dependsOn(core.jvm)
