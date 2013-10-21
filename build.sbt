name := "ScalaHDL"

organization := "com.liyaos"

version := "0.0.1"

scalaVersion := "2.10.2"

scalacOptions += "-Xexperimental"

libraryDependencies ++= Seq(
		    "org.scalatest" %% "scalatest" % "1.9.2" % "test",
		    "org.scala-lang" % "scala-reflect" % scalaVersion.value)