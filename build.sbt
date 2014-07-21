lazy val scalahdl = project
  .in(file("."))
  .aggregate(lib, parser)
  .settings(common: _*)

lazy val lib = project
  .in(file("scalahdl"))
  .settings(common: _*)
  .settings(name := "ScalaHDL",
    autoCompilerPlugins := true,
    scalacOptions ++= Seq("-Xexperimental",
      "-P:continuations:enable"),
    libraryDependencies ++= Seq(
      "org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.2.0",
      compilerPlugin("org.scala-lang.plugins" % "continuations" % scalaVersion.value)))

lazy val parser = project
  .in(file("parser"))
  .settings(common: _*)
  .settings(name := "ScalaHDL Parser")

lazy val plugin = project
  .in(file("sbt-ScalaHDL"))
  .dependsOn(lib, parser)
  .settings(common: _*)
  .settings(name := "ScalaHDL Sbt plugin",
    sbtPlugin := true)

lazy val examples = project
  .in(file("examples"))
  .dependsOn(lib, plugin)
  .settings(common: _*)
  .settings(name := "ScalaHDL Examples")

def common = Seq(
  organization := "com.liyaos",
  version := "0.0.2",
  scalaVersion := "2.10.2",
  libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test")
