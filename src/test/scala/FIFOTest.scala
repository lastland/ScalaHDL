import org.scalatest.FunSuite

import ScalaHDLExample.FIFO.FIFO
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object FifoTestBench extends FIFO {

  override val WIDTH = 3
  override val DEPTH = 2

  val INPUT = List(0, 7, 0, 3, 0, 1, 0, 0).iterator

  defMod.Bench('clk, 'rst, 'input, 'output) {
    delay(1) {
      cycle('clk)
    }

    sync(0) {
      'rst := 0
      'input := INPUT.next()
    }
  }
}

class FifoTest extends FunSuite with TestHelper {
  test("test fifo") {
    val clk = bool(0)
    val rst = bool(1)
    val input = unsigned(0, FifoTestBench.WIDTH)
    val output = unsigned(0, FifoTestBench.WIDTH)

    val OUTPUT = List(0, 0, 0, 7, 0, 3, 0, 1).iterator

    val sim = Simulator(FifoTestBench,
      module('fifo, clk, rst, input, output),
      module('Bench, clk, rst, input, output))
    sim.setTrace("fifo.vcd")
    sim since 0 until 16 every 2 run {
      assert(clk === 0)
    }
    sim since 1 until 16 every 2 run {
      assert(clk === 1)
      assert(output.value === OUTPUT.next())
    }
    sim test
  }
}
