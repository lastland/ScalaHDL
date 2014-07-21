import org.scalatest.FunSuite

import ScalaHDL.Parser.Compiler

class CompilerTest extends FunSuite {

  test("test a simple program") {
    val compiler = new Compiler("/Users/lastland/workspace/ScalaHDL", "/Users/lastland/workspace/ScalaHDL")

    compiler.compile("adder.shdl", "adder.scala")
  }

}
