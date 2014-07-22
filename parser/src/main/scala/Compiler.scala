package ScalaHDL.Parser

import scala.io.Source
import java.io._

class Compiler(sourcePath: String = ".", destinationPath: String = ".") {
  val parser = new ScalaHDLParser

  def compile(inputFileName: String, outputFileName: String): File = {
    println(sourcePath + "/" + inputFileName)
    val source = Source.fromFile(sourcePath + "/" + inputFileName)
    val code = source.mkString("")
    source.close

    val res = parser(code) match {
      case Some(mod) => mod.generate
      case _ => throw new RuntimeException("fail to parse!")
    }

    val outfile = new File(destinationPath + "/" + outputFileName)
    val out = new PrintWriter(outfile)
    out.print(res)
    out.close
    outfile
  }
}
