package ScalaHDLExample.Arithmetic.Mult

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signals._

trait Multiplier extends ScalaHDL {
  defMod.mult('clk, 'rst, 'a, 'b, 'z) {
    sync(1).mult {
      when ('rst is 1) {
        'z := 0
      } .otherwise {
        'z := 'a * 'b
      }
    }
  }
}

object Main extends Multiplier {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val a = signed(0, 4)
    val b = signed(0, 4)
    val z = signed(0, 8)

    println(convert('mult, clk, rst, a, b, z))
  }
}
