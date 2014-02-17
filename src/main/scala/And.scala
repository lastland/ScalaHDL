package ScalaHDLExample.Gate.And

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.HDLType
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Simulation.Simulator

trait And extends ScalaHDL {
  defMod.and('clk, 'rst, 'a, 'b, 'z) {
    sync(1).and {
      when ('rst is 1) {
        'z := 0
      } .otherwise {
        'z := 'a && 'b
      }
    }
  }
}

object Main extends And {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val a = bool(0)
    val b = bool(0)
    val z = bool(0)

    println(convert('and, clk, rst, a, b, z))
  }
}
