import org.scalatest.FunSuite

import ScalaHDLExample.FSM.Moore.Moore
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object MooreTestBench extends Moore {
  defMod.Bench('clk, 'rst) {
    delay(1) {
      cycle('clk)
    }

    sync(0) {
      'rst := 0
    }
  }
}

class MooreTest extends FunSuite with TestHelper {
  test("test moore") {
    val clk = bool(0)
    val rst = bool(1)
    val cars_green = bool(0)
    val cars_yellow = bool(0)
    val cars_red = bool(0)
    val ppl_green = bool(0)
    val ppl_yellow = bool(0)
    val ppl_red = bool(0)

    val sim = Simulator(MooreTestBench,
      module('moore, clk, rst,
      cars_green, cars_yellow, cars_red,
      ppl_green, ppl_yellow, ppl_red),
      module('Bench, clk, rst))
    sim.setTrace("moore.vcd")

    sim since 1 until 40 every 16 run {
      assert(cars_yellow.value === 1)
    }
    sim since 3 until 40 every 16 run {
      assert(cars_yellow.value === 0)
    }
    sim since 3 until 40 every 16 run {
      assert(cars_red.value === 1)
    }
    sim since 11 until 40 every 16 run {
      assert(cars_red.value === 0)
    }
    sim since 11 until 40 every 16 run {
      assert(cars_green.value === 1)
    }
    sim since 17 until 40 every 16 run {
      assert(cars_green.value === 0)
    }

    sim since 3 until 40 every 16 run {
      assert(ppl_green.value === 1)
    }
    sim since 9 until 40 every 16 run {
      assert(ppl_green.value === 0)
    }
    sim since 9 until 40 every 16 run {
      assert(ppl_yellow.value === 1)
    }
    sim since 11 until 40 every 16 run {
      assert(ppl_yellow.value === 0)
    }
    sim since 11 until 40 every 16 run {
      assert(ppl_red.value === 1)
    }
    sim since 19 until 40 every 16 run {
      assert(ppl_red.value === 0)
    }

    sim test
  }
}
