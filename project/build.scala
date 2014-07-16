import sbt._
import Keys._

object ScalaHDLBuild extends Build {
  lazy val main = Project("main", file(".")) dependsOn(scalahdlSub)
  lazy val scalahdlSub = Project("scalahdl", file("scalahdl"))
  lazy val scalahdlParserSub = Project("parser", file("parser"))
}
