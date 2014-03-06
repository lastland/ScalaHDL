import org.scalatest.FunSuite
import scala.util.Random

import ScalaHDLExample.BitonicSort.BitonicSort
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object BitonicSortTestBench extends BitonicSort {
  defMod.Bench('clk, 'a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7,
    'z0, 'z1, 'z2, 'z3, 'z4, 'z5, 'z6, 'z7) {
    delay(10).clkGen {
      cycle('clk)
    }

    sync(0).stimulus {
      for (a <- List('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)) {
        a := Random.nextInt(16)
      }
    }
  }
}

class BitonicSortTest extends FunSuite with TestHelper {
  test("test bitonic sort") {
    val a0 = unsigned(0, 4)
    val a1 = unsigned(0, 4)
    val a2 = unsigned(0, 4)
    val a3 = unsigned(0, 4)
    val a4 = unsigned(0, 4)
    val a5 = unsigned(0, 4)
    val a6 = unsigned(0, 4)
    val a7 = unsigned(0, 4)

    val z0 = unsigned(0, 4)
    val z1 = unsigned(0, 4)
    val z2 = unsigned(0, 4)
    val z3 = unsigned(0, 4)
    val z4 = unsigned(0, 4)
    val z5 = unsigned(0, 4)
    val z6 = unsigned(0, 4)
    val z7 = unsigned(0, 4)

    val clk = bool(0)

    val sim = Simulator(BitonicSortTestBench,
      module('sort8, a0, a1, a2, a3, a4, a5, a6, a7,
        z0, z1, z2, z3, z4, z5, z6, z7),
      module('Bench, clk, a0, a1, a2, a3, a4, a5, a6, a7,
        z0, z1, z2, z3, z4, z5, z6, z7))
    sim.setTrace("bs.vcd")
    sim since 0 to 1000 every 20 run {
      assert(clk === 0)
    }
    sim since 10 to 1000 every 20 run {
      assert(clk === 1)
      val lst = List(a0, a1, a2, a3, a4, a5, a6, a7).map(_.value)
      val rst = List(z0, z1, z2, z3, z4, z5, z6, z7).map(_.value)
      assert(rst === lst.sorted)
    }
    sim test
  }
}
