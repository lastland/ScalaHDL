package ScalaHDLExample.Gate.Xor

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signals._

trait Xor extends ScalaHDL {
  defMod.xor('clk, 'rst, 'a, 'b, 'z) {
    sync(1).xor {
      when ('rst is 1) {
        'z := 0
      } .otherwise {
        'z := 'a ^ 'b
      }
    }
  }
}

object Main extends Xor {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val a = bool(0)
    val b = bool(0)
    val z = bool(0)

    println(convert('xor, clk, rst, a, b, z))
  }
}
