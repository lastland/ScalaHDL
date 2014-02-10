package ScalaHDLExample.FlipFlop
import scala.util.Random

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Simulation.Simulator

object Main extends ScalaHDL {
  defMod.FlipFlop('d, 'q, 'clk) {
    sync(1).logic {
      'q := 'd
    }
  }

  defMod.Bench('d, 'q, 'clk) {
    delay(10).clkGen {
      cycle('clk)
    }

    sync(0).stimulus {
      'd := Random.nextInt(2)
    }
  }


  def main(args: Array[String]) {
    val q = bool(0)
    val d = bool(1)
    val clk = bool(0)
    println(convert('FlipFlop, d, q, clk))
    val sim = Simulator(this,
      module('FlipFlop, d, q, clk),
      module('Bench, d, q, clk))
    sim.simulate(1000, "dff.vcd")
    sim.stop()
  }
}
