package ScalaHDL.Parser

import scala.io.Source
import java.io._

class Compiler(sourcePath: String = ".", destinationPath: String = ".") {
  val parser = new ScalaHDLParser

  def compile(inputFileName: String, outputFileName: String) {
    val source = Source.fromFile(sourcePath + "/" + inputFileName)
    val code = source.mkString("")
    source.close

    val res = parser(code) match {
      case Some(mod) => mod.generate
      case _ => throw new RuntimeException("fail to parse!")
    }

    val out = new PrintWriter(destinationPath + "/" + outputFileName)
    out.print(res)
    out.close
  }
}
