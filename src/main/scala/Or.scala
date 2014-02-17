package ScalaHDLExample.Gate.Or

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.HDLType
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Simulation.Simulator

trait Or extends ScalaHDL {
  defMod.or('clk, 'rst, 'a, 'b, 'z) {
    sync(1).or {
      when ('rst is 1) {
        'z := 0
      } .otherwise {
        'z := 'a || 'b
      }
    }
  }
}

object Main extends Or {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val a = bool(0)
    val b = bool(0)
    val z = bool(0)

    println(convert('or, clk, rst, a, b, z))
  }
}
