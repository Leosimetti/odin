ThisBuild / scalaVersion := "2.13.6"

ThisBuild / name := "odin-project"
ThisBuild / organization := "org.polystat.odin"
ThisBuild / organizationName := "polystat"
ThisBuild / organizationHomepage := Some(url("https://github.com/polystat"))
ThisBuild / homepage := Some(url("https://github.com/polystat/odin"))
ThisBuild / description :=
  """Odin (object dependency inspector) — static analyzer for EO source code
    |that detects OOP-related bugs.""".stripMargin

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

lazy val publishSettings = Seq(
  scmInfo := Some(ScmInfo(
    url("https://github.com/polystat/odin"),
    "scm:git@github.com:polystat/odin.git"
  )),
  developers := List(
    Developer(
      id = "sitiritis",
      name = "Tymur Lysenko",
      email = "nanotimcool@gmail.com",
      url = url("https://github.com/Sitiritis"),
    ),
    Developer(
      id = "nikololiahim",
      name = "Mihail Olokin",
      email = "olomishcak@gmail.com",
      url = url("https://github.com/nikololiahim"),
    ),
  ),
  licenses := List("MIT" -> url("https://mit-license.org")),
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://s01.oss.sonatype.org/"

    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact := true,
  publishMavenStyle := true,
)

lazy val commonSettings = Compiler.settings ++ Seq(
  resolvers += Opts.resolver.sonatypeSnapshots
)

lazy val odin = project
  .in(file("."))
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(
    utils,
    core,
    parser,
    analysis,
    backends,
    interop,
  )
  .aggregate(
    utils,
    core,
    parser,
    analysis,
    backends,
    interop,
  )
  .settings(
    name := "odin",
  )

lazy val utils = project
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "utils",
    libraryDependencies ++= Dependencies.common,
  )

lazy val core = project
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(utils)
  .settings(
    name := "core",
    libraryDependencies ++= Dependencies.core,
  )

lazy val parser = project
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(core)
  .settings(
    name := "parser",
    libraryDependencies ++= Dependencies.parser
  )

lazy val analysis = project
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(
    parser,
    core
  )
  .settings(
    name := "analysis"
  )

val backendsBaseDirectory: File = file("backends")
lazy val backends: Project = project
  .in(backendsBaseDirectory)
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(`eolang-backend`)
  .aggregate(`eolang-backend`)
  .settings(
    name := "backends",
  )

lazy val `eolang-backend` = project
  .in(backendsBaseDirectory / "eolang")
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(core)
  .settings(
    name := "eolang-backend",
  )

lazy val interop = project
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn()
  .settings(
    name := "interop"
  )

lazy val sandbox = project
  .settings(commonSettings)
  // Remove strict checks, so that it is easy to modify sandbox when developing
  .settings(scalacOptions ~= (_.filterNot(Compiler.consoleOptionsToRemove)))
  .settings(noPublishSettings)
  .dependsOn(`odin`)
  .settings(
    name := "sandbox",
    libraryDependencies ++= Dependencies.common,
  )
