import org.scalatest.FunSuite

import ScalaHDLExample.Gate.And.And
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object AndTestBench extends And {
  val A = List(0, 0, 1, 1, 0).iterator
  val B = List(0, 1, 0, 1, 0).iterator
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

class AndTest extends FunSuite with TestHelper {
  test("test and") {
    val clk = bool(0)
    val rst = bool(1)
    val a = bool(0)
    val b = bool(0)
    val z = bool(0)

    val Z = List(0, 0, 0, 1).iterator

    val sim = Simulator(AndTestBench,
      module('and, clk, rst, a, b, z),
      module('Bench, clk, rst, a, b, z))
    sim.setTrace("and.vcd")
    sim since 0 until 10 every 2 run {
      assert(clk === 0)
    }
    sim since 3 until 10 every 2 run {
      assert(clk === 1)
      assert(z.value === Z.next)
    }
    sim test
  }
}
