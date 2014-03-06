package ScalaHDLExample.FIFO

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signals._

trait FIFO extends ScalaHDL {

  val WIDTH = 1
  val DEPTH = 1

  defMod.fifo('clk, 'rst, 'input, 'output) {
    sync(1).fifo {
      val fifo_registers =
        (for (i <- 0 until DEPTH) yield unsigned(0, WIDTH)).map(toHDLType)

      when ('rst is 1) {
        'output := 0
        for (i <- 0 until DEPTH)
          fifo_registers(i) := 0
      } .otherwise {
        fifo_registers(0) := 'input
        'output := fifo_registers(DEPTH - 1)
        if (DEPTH > 1) {
          for (i <- 0 until DEPTH - 1)
            fifo_registers(i + 1) := fifo_registers(i)
        }
      }
    }
  }
}

object Main extends FIFO {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val input = unsigned(0, WIDTH)
    val output = unsigned(0, WIDTH)

    println(convert('fifo, clk, rst, input, output))
  }
}
