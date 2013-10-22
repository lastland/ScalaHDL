package ScalaHDL

import ScalaHDL.DataType._

object Main extends ScalaHDL { 
  def main(args: Array[String]) { 
    module.adder('a, 'b, 'res) { 
      'res := 'a + 'b
      'res := 'res + 'a
    }
    val a = Signal(3, 3)
    val b = Signal(2, 3)
    val res = Signal(0, 3)
    println(convert('adder, a, b, res))
  }
}
