import org.scalatest.FunSuite

import ScalaHDLExample.ROM.Rom
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object RomTestBench extends Rom {

  val WL_ADDR = 2
  val WL_DATA = 3

  val ADDR = List(1, 2, 3).iterator

  defMod.Bench('clk, 'addr, 'dout) {
    delay(1) {
      cycle('clk)
    }

    delay(3) {
      'addr := ADDR.next()
    }
  }
}

class RomTest extends FunSuite with TestHelper {
  test("test rom") {
    val clk = bool(0)
    val addr = unsigned(0, RomTestBench.WL_ADDR)
    val dout = unsigned(0, RomTestBench.WL_DATA)

    val DOUT = List(0, 2, 0, 3, 0, 6, 0, 7).iterator

    val sim = Simulator(RomTestBench,
      module('rom, clk, addr, dout),
      module('Bench, clk, addr, dout))
    sim.setTrace("rom.vcd")
    sim since 0 until 12 every 2 run {
      assert(clk === 0)
    }
    sim since 1 until 12 every 2 run {
      assert(clk === 1)
    }
    sim test
  }
}
