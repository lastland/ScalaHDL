import org.scalatest.FunSuite

import ScalaHDLExample.Bin2Gray.Bin2Gray
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Test.TestHelper

object Bin2grayTestBench extends Bin2Gray {

  override val width = 3

  val B = (0 until 16).toList.iterator

  defMod.Bench('clk, 'g, 'b) {
    delay(1) {
      cycle('clk)
    }

    sync(0) {
      'b := B.next()
    }
  }
}

class Bin2grayTest extends FunSuite with TestHelper {
  test("test bin2gray") {
    val clk = bool(0)
    val b = unsigned(0, Bin2grayTestBench.width + 1)
    val g = unsigned(0, Bin2grayTestBench.width + 1)

    val Z = List(0, 0, 1, 1, 2, 15, 15, 30).iterator

    val sim = Simulator(Bin2grayTestBench,
      module('bin2gray, g, b),
      module('Bench, clk, g, b))
    sim.setTrace("bin2gray.vcd")
    sim since 0 until 32 every 2 run {
      assert(clk === 0)
    }
    sim since 1 until 32 every 2 run {
      assert(clk === 1)
    }
    sim test
  }
}
