package ScalaHDL

import ScalaHDL.DataType._

object Main extends ScalaHDL {
  def main(args: Array[String]) {
    sync(clk = 1)
    module.logic('d, 'q) {
      'q := 'd
    }
    val q = Signal(3, 3)
    val d = Signal(2, 3)
    println(convert('logic, d, q))
  }
}
