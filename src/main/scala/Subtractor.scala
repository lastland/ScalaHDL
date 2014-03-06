package ScalaHDLExample.Arithmetic.Sub

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signals._

trait Subtractor extends ScalaHDL {

  val size = 4

  defMod.sub('clk, 'rst, 'a, 'b, 'z) {
    sync(1).sub {
      when ('rst is 1) {
        'z := 0
      } .otherwise {
        'z := ('a - 'b) % math.pow(2, size + 1).toInt
      }
    }
  }
}

object Main extends Subtractor {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val a = signed(0, 4)
    val b = signed(0, 4)
    val z = signed(0, 5)

    println(convert('sub, clk, rst, a, b, z))
  }
}
