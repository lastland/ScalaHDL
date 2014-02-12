import org.scalatest.FunSuite

import ScalaHDLExample.Comparator.Comp
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object CompTestBench extends Comp {
  val A = List(0, 0, 1, 1, 31, 0, 31).iterator
  val B = List(0, 1, 0, 1, 0, 31, 31).iterator
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

class CompTest extends FunSuite with TestHelper {
  test("test comparator") {
    val clk = bool(0)
    val rst = bool(1)
    val a = unsigned(0, 5)
    val b = unsigned(0, 5)
    val z = signed(0, 2)

    val Z = List(0, 0, -1, 1, 0, 1, -1, 0).iterator

    val sim = Simulator(CompTestBench,
      module('comp, clk, rst, a, b, z),
      module('Bench, clk, rst, a, b, z))
    sim.setTrace("comp.vcd")
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
