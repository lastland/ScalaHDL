package ScalaHDLExample.RAM

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.HDLType
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._

trait Ram extends ScalaHDL {

  val WIDTH = 64
  val DEPTH = 2048

  defMod.ram('clk, 'we, 'addr, 'din, 'dout) {

    val mem = toHDLList((for (i <- 1 to DEPTH) yield unsigned(0, WIDTH)).toList)

    sync(1).ram {
      when ('we is 1) {
        mem('addr) := 'din
      }
      'dout := mem('addr)
    }
  }
}

object Main extends Ram {

  val WL_ADDR = 2
  val WL_DATA = 3

  override val WIDTH = math.pow(2, WL_DATA).toInt
  override val DEPTH = math.pow(2, WL_ADDR).toInt

  def main(args: Array[String]) {
    val clk = bool(0)
    val we = bool(0)
    val addr = unsigned(0, WL_ADDR)
    val din = unsigned(0, WL_DATA)
    val dout = unsigned(0, WL_DATA)

    println(convert('ram, clk, we, addr, din, dout))
  }
}
