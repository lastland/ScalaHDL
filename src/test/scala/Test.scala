import org.scalatest.FunSuite

import ScalaHDLExample.FlipFlop.Main
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.SignalMaker._
import ScalaHDL.Test.TestHelper

class MainTest extends FunSuite with TestHelper {
  test("test dff 1") {
    val q = signal(0)
    val d = signal(1)
    val clk = signal(0)
    val sim = Simulator(Main,
      module('logic, d, q, clk),
      module('clkGen, clk),
      module('stimulus, d, clk))

    sim.simulate(10)
    assert(clk === 1)
    for (i <- 1 to 100) {
      sim.continue(10)
      assert(clk === 0)
      sim.continue(10)
      assert(clk === 1)
      assert(q.value === d.value)
    }
    sim.stop()
  }

  test("test dff 2") {
    val q = signal(0)
    val d = signal(1)
    val clk = signal(0)
    val sim = Simulator(Main,
      module('logic, d, q, clk),
      module('clkGen, clk),
      module('stimulus, d, clk))

    sim since 0 to 1000 every 20 run {
      assert(clk === 0)
    }
    sim since 10 to 1000 every 20 run {
      assert(clk === 1)
      assert(q.value === d.value)
    }
    sim test
  }
}
