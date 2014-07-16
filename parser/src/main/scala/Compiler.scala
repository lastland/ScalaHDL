package ScalaHDL.Parser

import scala.io.Source
import java.io._

class Compiler(sourcePath: String = ".", destinationPath: String = ".") {
  val parser = new ScalaHDLParser

  val headers = """|import ScalaHDL.Core.ScalaHDL
                   |import ScalaHDL.Core.DataType.Signals._
                   |""".stripMargin

  def compile(inputFileName: String, outputFileName: String) {
    val source = Source.fromFile(sourcePath + "/" + inputFileName)
    val code = source.mkString("")
    source.close

    val res = headers + (parser(code) match {
      case Some(mod) => mod.generate
      case _ => throw new RuntimeException("fail to parse!")
    })

    val out = new PrintWriter(destinationPath + "/" + outputFileName)
    out.print(res)
    out.close
  }
}
