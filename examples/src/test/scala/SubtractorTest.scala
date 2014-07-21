import org.scalatest.FunSuite

import ScalaHDLExample.Arithmetic.Sub.Subtractor
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object SubtractorTestBench extends Subtractor {
  val A = List(0, 0, 1, 1, 15, 0, 15).iterator
  val B = List(0, 1, 0, 1, 0, 15, 15).iterator
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

class SubtractorTest extends FunSuite with TestHelper {
  test("test subtractor") {
    val clk = bool(0)
    val rst = bool(1)
    val a = unsigned(0, SubtractorTestBench.size)
    val b = unsigned(0, SubtractorTestBench.size)
    val z = unsigned(0, SubtractorTestBench.size + 1)

    val Z = List(0, 0, 31, 1, 0, 15, 17, 0).iterator

    val sim = Simulator(SubtractorTestBench,
      module('sub, clk, rst, a, b, z),
      module('Bench, clk, rst, a, b, z))
    sim.setTrace("sub.vcd")
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
