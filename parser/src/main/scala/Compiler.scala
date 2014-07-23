package ScalaHDL.Parser

import scala.io.Source
import java.io._

class CompilerException(fileName: String)
    extends Exception("Compile error! Failed to compile " + fileName)

class Compiler(sourcePath: String = ".", destinationPath: String = ".") {
  val parser = new ScalaHDLParser

  def compile(inputFileName: String, outputFileName: String): File = {
    val source = Source.fromFile(sourcePath + "/" + inputFileName)
    val code = source.mkString("")
    source.close

    val res = parser(code) match {
      case Some(mod) => mod.generate
      case _ => throw new CompilerException(inputFileName)
    }

    val outfile = new File(destinationPath + "/" + outputFileName)
    val out = new PrintWriter(outfile)
    out.print(res)
    out.close
    outfile
  }
}
