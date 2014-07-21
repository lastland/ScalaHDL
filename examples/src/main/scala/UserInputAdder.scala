package ScalaHDLExample.UserInput.Add

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signals._

object Main extends ScalaHDL {
  defMod.adder('clk, 'rst, 'a, 'b, 'z) {
    sync(1).add {
      when ('rst is 1) {
        'z := 0
      } .otherwise {
        'z := 'a + 'b
      }
    }

    defMod.Bench('clk, 'rst, 'a, 'b, 'z) {
      delay(1) {
        cycle('clk)
      }

      sync(0) {
        'rst := 0
        'a := Console.readInt()
        'b := Console.readInt()
      }
    }
  }

  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val a = signed(0, 5)
    val b = signed(0, 5)
    val z = signed(0, 6)

    val sim = Simulator(Main,
      module('adder, clk, rst, a, b, z),
      module('Bench, clk, rst, a, b, z))
    sim.setTrace("add-ui.vcd")
    sim.simulate(10)
    sim.stop()
  }
}
