import org.scalatest.FunSuite
import scala.util.Random

import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object FlipFlopTestBench extends ScalaHDLExample.FlipFlop.FlipFlop {
  defMod.Bench('d, 'q, 'clk) {
    delay(10).clkGen {
      cycle('clk)
    }

    sync(0).stimulus {
      'd := Random.nextInt(2)
    }
  }
}

class FlipFlopTest extends FunSuite with TestHelper {
  test("test dff 1") {
    val q = bool(0)
    val d = bool(1)
    val clk = bool(0)
    val sim = Simulator(FlipFlopTestBench,
      module('FlipFlop, d, q, clk),
      module('Bench, d, q, clk))

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
    val q = bool(0)
    val d = bool(1)
    val clk = bool(0)
    val sim = Simulator(FlipFlopTestBench,
      module('FlipFlop, d, q, clk),
      module('Bench, d, q, clk))
    sim.setTrace("dff.vcd")
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
