import scala.scalanative.build._
import org.scalajs.linker.interface.ESVersion


ThisBuild / version := "0.2.0"

ThisBuild / scalaVersion := "3.2.1"

ThisBuild / javaOptions += "-Xss1G"
ThisBuild / javaOptions += "-Xmx8G"
ThisBuild / Test / fork := false


lazy val root = crossProject(JSPlatform, JVMPlatform, NativePlatform).withoutSuffixFor(JVMPlatform)
  .in(file("."))
  .settings(
    name := "CZ2",
    idePackagePrefix := Some("be.adamv.cz2"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M7" % Test,
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
