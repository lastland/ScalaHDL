package ScalaHDLExample.ROM

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signals._

trait Rom extends ScalaHDL {

  val WIDTH = 64
  val DEPTH = 2048
  val DATA = HDLValueList(2, 3, 6, 7)

  defMod.rom('clk, 'addr, 'dout) {
    sync(0).rom {
      'dout := DATA('addr)
    }
  }
}

object Main extends Rom {

  val WL_ADDR = 2
  val WL_DATA = 3

  override val WIDTH = math.pow(2, WL_DATA).toInt
  override val DEPTH = math.pow(2, WL_ADDR).toInt

  def main(args: Array[String]) {
    val clk = bool(0)
    val addr = unsigned(0, WL_ADDR)
    val dout = unsigned(0, WL_DATA)

    println(convert('rom, clk, addr, dout))
  }
}
