lazy val lib = ProjectRef(file(".."), "lib")

lazy val examples = project
  .in(file("."))
  .dependsOn(lib)
  .enablePlugins(SbtScalaHDL)
  .settings(name := "ScalaHDL Examples",
    organization := "com.liyaos",
    version := "0.0.2",
    scalaVersion := "2.10.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test")
