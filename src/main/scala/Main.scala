package ScalaHDL

import scala.io.Source

object Main extends SHDLParser { 
  def main(args: Array[String]) { 
    val text = """|def add(a, b, res) {
		  |  res = a + b
		  |}
		  |
		  |def main() {
		  |  a = Signal(Int, 3)
		  |  b = Signal(Int, 3)
		  |  c = Signal(Int, 3)
		  |  convert(add, a, b, c)
		  |}""".stripMargin
    val p = (new SHDLParser).run(text) match { 
      case Success(pro, _) => pro
      case _ => throw new RuntimeException
    }
    println(p.modules)
  }
}
