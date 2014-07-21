package ScalaHDLExample.Gate.Nor

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signals._

trait Nor extends ScalaHDL {
  defMod.nor('clk, 'rst, 'a, 'b, 'z) {
    sync(1).nor {
      when ('rst is 1) {
        'z := 0
      } .otherwise {
        'z := !('a || 'b)
      }
    }
  }
}

object Main extends Nor {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val a = bool(0)
    val b = bool(0)
    val z = bool(0)

    println(convert('nor, clk, rst, a, b, z))
  }
}
