lazy val lib = ProjectRef(file(".."), "lib")

lazy val parser = ProjectRef(file(".."), "parser")

lazy val plugin = project
  .in(file("."))
  .dependsOn(lib, parser)
  .settings(name := "ScalaHDL Sbt plugin",
    sbtPlugin := true,
    organization := "com.liyaos",
    version := "0.0.1",
    scalaVersion := "2.10.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test")
