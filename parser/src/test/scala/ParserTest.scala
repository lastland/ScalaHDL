import org.scalatest.FunSuite
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.combinator.Parsers

import ScalaHDL.Parser.ScalaHDLParser

class ScalaHDLParserTest extends FunSuite {

  val testProgram = """|defmod Adder(a, b, z) {
                       |  @sync(1)
                       |  adder {
                       |    z := a + b
                       |  }
                       |}""".stripMargin

  val testProgram2 = """|defmod Adder(a, b, z) {
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
