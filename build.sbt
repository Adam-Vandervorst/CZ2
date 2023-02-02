ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.2.1"

ThisBuild / javaOptions += "-Xss1G"
ThisBuild / javaOptions += "-Xmx8G"
ThisBuild / Test / fork := false

lazy val root = crossProject(JSPlatform, JVMPlatform, NativePlatform).withoutSuffixFor(JVMPlatform)
  .in(file("."))
  .settings(
    name := "CZ2",
    idePackagePrefix := Some("be.adamv.cz2"),
    ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

publishTo := Some(Resolver.file("local-ivy", file("~")))
