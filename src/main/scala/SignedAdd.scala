package ScalaHDLExample.Arithmetic.SignedAdd

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signals._

trait Adder extends ScalaHDL {
  defMod.adder('clk, 'rst, 'a, 'b, 'z) {
    sync(1).add {
      when ('rst is 1) {
        'z := 0
      } .otherwise {
        'z := 'a + 'b
      }
    }
  }
}

object Main extends Adder {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val a = signed(0, 5)
    val b = unsigned(0, 4)
    val z = signed(0, 6)

    println(convert('adder, clk, rst, a, b, z))
  }
}
