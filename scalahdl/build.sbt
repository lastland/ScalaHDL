name := "ScalaHDL"

organization := "com.liyaos"

version := "0.0.1"

scalaVersion := "2.10.2"

autoCompilerPlugins := true

scalacOptions ++= Seq("-Xexperimental",
  "-P:continuations:enable")

libraryDependencies ++= Seq(
  "org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.2.0",
  "org.scalatest" %% "scalatest" % "1.9.2" % "test",
  compilerPlugin("org.scala-lang.plugins" % "continuations" % scalaVersion.value))
