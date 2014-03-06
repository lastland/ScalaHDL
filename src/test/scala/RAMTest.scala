import org.scalatest.FunSuite

import ScalaHDLExample.RAM.Ram
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object RamTestBench extends Ram {

  val WL_ADDR = 2
  val WL_DATA = 3

  val ADDR = List(1, 2, 3).iterator
  val DIN = List(3, 6, 7).iterator

  defMod.Bench('clk, 'we, 'addr, 'din, 'dout) {
    delay(1) {
      cycle('clk)
    }

    delay(2) {
      cycle('we)
    }

    delay(4) {
      'addr := ADDR.next()
      'din := DIN.next()
    }
  }
}

class RamTest extends FunSuite with TestHelper {
  test("test ram") {
    val clk = bool(0)
    val we = bool(1)
    val addr = unsigned(0, RamTestBench.WL_ADDR)
    val din = unsigned(2, RamTestBench.WL_DATA)
    val dout = unsigned(0, RamTestBench.WL_DATA)

    val DOUT = List(0, 2, 0, 3, 0, 6, 0, 7).iterator

    val sim = Simulator(RamTestBench,
      module('ram, clk, we, addr, din, dout),
      module('Bench, clk, we, addr, din, dout))
    sim.setTrace("ram.vcd")
    sim since 0 until 16 every 2 run {
      assert(clk === 0)
    }
    sim since 1 until 16 every 2 run {
      assert(clk === 1)
    }
    sim test
  }
}
