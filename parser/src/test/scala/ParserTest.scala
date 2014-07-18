import org.scalatest.FunSuite
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.combinator.Parsers

import ScalaHDL.Parser.ScalaHDLParser

class ScalaHDLParserTest extends FunSuite {

  val testProgram = """|@GeAdder
                       |defmod Adder(clk, a, b, z) {
                       |  @sync(1)
                       |  adder {
                       |    z := a + b
                       |  }
                       |}
                       |
                       |@main {
                       |  clk = bool(0)
                       |  a = signed(0, 5)
                       |  b = signed(0, 5)
                       |  z = signed(0, 6)
                       |
                       |  compile(adder(clk, a, b, z), "Adder.v")
                       |} """.stripMargin

  val testProgram2 = """|@GeTest
                        |defmod Adder(a, b, z) {
                        |  @sync(1)
                        |  adder {
                        |    @{
                        |     if () {
                        |      z := a + b
                        |      z := a + b
                        |      z := a + b
                        |    } else {
                        |      z := a - b
                        |    }
                        |    }
                        |  }
                        |}
                        |
                        |@main {
                        |  clk = bool(0)
                        |  a = signed(0, 5)
                        |  b = signed(0, 5)
                        |  z = signed(0, 6)
                        |
                        |  compile(adder(clk, a, b, z), "Adder.v")
                        |}""".stripMargin


  test("test a simple assignment") {
    val p = new ScalaHDLParser
    p(testProgram) match {
      case Some(mod) => println(mod.generate)
      case _ => println("wrong")
    }
  }

  test("test a simple assignment with scala code") {
    val p = new ScalaHDLParser
    p(testProgram2) match {
      case Some(mod) => println(mod.generate)
      case _ => println("wrong2")
    }
  }
}
