// BSP fixture for scala-psi-host: two modules, a module dependency and per-module compiler options.
ThisBuild / scalaVersion := "2.13.18"

lazy val core = project

lazy val app = project
  .dependsOn(core)
  .settings(scalacOptions += "-Xsource:3")
