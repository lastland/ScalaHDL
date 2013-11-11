package ScalaHDL
import scala.util.Random

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType._
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
    val q = Signal(0)
    val d = Signal(1)
    val clk = Signal(0)
    val sim = new Simulator(this)
    sim.simulate(
      1000,
      module('logic, q, d, clk),
      module('clkGen, clk),
      module('stimulus, d, clk))
  }
}
