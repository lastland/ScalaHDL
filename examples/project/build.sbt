lazy val plugin = uri("../../sbt-ScalaHDL")

lazy val root = project.in(file(".")).dependsOn(plugin)
