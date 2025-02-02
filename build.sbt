import scala.scalanative.build.*
import org.scalajs.linker.interface.ESVersion


ThisBuild / version := "0.2.14"
ThisBuild / organization := "be.adamv"

ThisBuild / scalaVersion := "3.4.2"

ThisBuild / javaOptions += "-Xss1G"
ThisBuild / javaOptions += "-Xmx8G"

lazy val root = crossProject(JSPlatform, JVMPlatform, NativePlatform).withoutSuffixFor(JVMPlatform)
  .in(file("."))
  .jvmSettings(
    ThisBuild / fork := true,
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M10" % Test,
  )
  .settings(
    name := "CZ2",
    idePackagePrefix := Some("be.adamv"),
    nativeConfig ~= {
      _.withLTO(LTO.thin)
        .withMode(Mode.releaseFull)
        .withGC(GC.commix)
    },
    Compile / fullLinkJS / scalaJSLinkerConfig ~= {  // set Global/scalaJSStage := FullOptStage
      _.withESFeatures(_.withESVersion(ESVersion.ES2021))
        .withClosureCompiler(true)
        .withCheckIR(true)
    },
    jsEnv := {
      import org.scalajs.jsenv.nodejs.NodeJSEnv
      new NodeJSEnv(
        NodeJSEnv.Config().withArgs(List("--stack-size=10000"))
      )
    },
    scalaJSUseMainModuleInitializer := true
  )


publishTo := Some(Resolver.file("local-ivy", file("~")))
