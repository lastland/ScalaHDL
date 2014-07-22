package ScalaHDL.sbt

import sbt._
import sbt.Keys._

import java.io.File
import ScalaHDL.Parser.Compiler

object SbtScalaHDL extends AutoPlugin {

  override def requires = plugins.JvmPlugin
  override def trigger = noTrigger

  override lazy val projectSettings = Seq(
    commands += helloCommand,
    sourceGenerators in Compile += Def.task {
      val shdlSource = sourceDirectory.value / "main" / "shdl"
      try {
        val sourceFileNames =
          shdlSource.listFiles.map(_.getName).filter(_.endsWith(".shdl"))

        val compiler = new Compiler(shdlSource.getPath,
          (sourceManaged in Compile).value.getPath)

        Seq(sourceFileNames.map(f => f.substring(0, f.size - 5)).map(n =>
          compiler.compile(n + ".shdl", n + ".scala")
        ): _*)
      } catch {
        case _: RuntimeException => println("Error!"); Seq()
        case _: Throwable => Seq()
      }
    }.taskValue
  )
  lazy val helloCommand =
    Command.command("hello") { (state: State) =>
      println("Hi!")
      state
    }
}
