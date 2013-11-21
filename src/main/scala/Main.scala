package ScalaHDL
import scala.util.Random

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.SignalMaker._
import ScalaHDL.Simulation.Simulator

object Main extends ScalaHDL {
  sync('clk is 1)
  defMod.logic('d, 'q, 'clk) {
    'q := 'd
  }

  delay(10)
  defMod.clkGen('clk) {
    cycle('clk)
  }

  sync('clk is 0)
  defMod.stimulus('d, 'clk) {
    'd := Random.nextInt(2)
  }

  def main(args: Array[String]) {
    val q = signal(0)
    val d = signal(1)
    val clk = signal(0)
    val sim = Simulator(this,
      module('logic, d, q, clk),
      module('clkGen, clk),
      module('stimulus, d, clk))
    sim.simulate(1000, "dff.vcd")
    sim.stop()
  }
}
