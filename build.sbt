ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"


ThisBuild / javaOptions += "-Xss1G"
ThisBuild / Test / fork := true

lazy val root = (project in file("."))
  .settings(
    name := "CZ2",
    idePackagePrefix := Some("be.adamv"),
    ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
