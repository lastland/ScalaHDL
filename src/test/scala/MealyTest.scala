import org.scalatest.FunSuite

import ScalaHDLExample.FSM.Mealy.Mealy
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object MealyTestBench extends Mealy {

  val CW = List(0, 0, 1, 0).iterator
  val PW = List(0, 1, 0, 0).iterator

  defMod.Bench('clk, 'rst, 'car_waiting, 'ppl_waiting) {
    delay(1) {
      cycle('clk)
    }

    sync(0) {
      'rst := 0
      'car_waiting := CW.next()
      'ppl_waiting := PW.next()
    }
  }
}

class MealyTest extends FunSuite with TestHelper {
  test("test mealy") {
    val clk = bool(0)
    val rst = bool(1)
    val car_waiting = bool(0)
    val ppl_waiting = bool(0)
    val cars_green = bool(0)
    val cars_yellow = bool(0)
    val cars_red = bool(0)
    val ppl_green = bool(0)
    val ppl_yellow = bool(0)
    val ppl_red = bool(0)

    val sim = Simulator(MealyTestBench,
      module('mealy, clk, rst,
        car_waiting, ppl_waiting,
        cars_green, cars_yellow, cars_red,
        ppl_green, ppl_yellow, ppl_red),
      module('Bench, clk, rst, car_waiting, ppl_waiting))
    sim.setTrace("mealy.vcd")

    sim since 1 until 4 every 1 run {
      assert(cars_green.value === 1)
    }
    sim since 4 until 7 every 1 run {
      assert(cars_green.value === 0)
    }
    sim since 5 until 7 every 1 run {
      assert(cars_red.value === 1)
    }
    sim since 4 until 5 every 1 run {
      assert(cars_yellow.value === 1)
    }

    sim since 1 until 5 every 1 run {
      assert(ppl_red.value === 1)
    }
    sim since 5 until 7 every 1 run {
      assert(ppl_red.value === 0)
    }
    sim since 5 until 6 every 1 run {
      assert(ppl_green.value === 1)
    }
    sim since 6 until 7 every 1 run {
      assert(ppl_yellow.value === 1)
    }

    sim test
  }
}
