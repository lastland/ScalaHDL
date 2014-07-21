package ScalaHDL.sbt

import sbt._
import sbt.Keys._

object SbtScalaHDL extends AutoPlugin {
  lazy val hello = taskKey[Unit]("Prints 'Hello World'")
  hello in Compile := println("Hello World")
}
