import org.scalatest.FunSuite

import ScalaHDLExample.Arithmetic.Mult.Multiplier
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object MultiplierTestBench extends Multiplier {
  val A = List(0, 0, 1, 1, 15, 1, 15).iterator
  val B = List(0, 1, 0, 1, 1, 15, 15).iterator
  defMod.Bench('clk, 'rst, 'a, 'b, 'z) {
    delay(1) {
      cycle('clk)
    }

    sync(0) {
      'rst := 0
      'a := A.next()
      'b := B.next()
    }
  }
}

class MultiplierTest extends FunSuite with TestHelper {
  test("test multiplier") {
    val clk = bool(0)
    val rst = bool(1)
    val a = unsigned(0, 4)
    val b = unsigned(0, 4)
    val z = unsigned(0, 8)

    val Z = List(0, 0, 0, 1, 15, 15, 225).iterator

    val sim = Simulator(MultiplierTestBench,
      module('mult, clk, rst, a, b, z),
      module('Bench, clk, rst, a, b, z))
    sim.setTrace("mult.vcd")
    sim since 0 until 14 every 2 run {
      assert(clk === 0)
    }
    sim since 1 until 14 every 2 run {
      assert(clk === 1)
      assert(z.value === Z.next)
    }
    sim test
  }
}
