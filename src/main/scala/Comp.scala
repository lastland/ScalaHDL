package ScalaHDLExample.Comparator

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.HDLType
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Simulation.Simulator

trait Comp extends ScalaHDL {
  defMod.comp('clk, 'rst, 'a, 'b, 'z) {
    sync(1).comp {
      when ('rst is 1) {
        'z := 0
      } .otherwise {
        when ('a > 'b) {
          'z := 1
        } .elsewhen ('a < 'b) {
          'z := -1
        } .otherwise {
          'z := 0
        }
      }
    }
  }
}

object Main extends Comp {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val a = unsigned(0, 5)
    val b = unsigned(0, 5)
    val z = signed(0, 2)

    println(convert('comp, clk, rst, a, b, z))
  }
}
