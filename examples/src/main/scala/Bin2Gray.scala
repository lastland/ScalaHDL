package ScalaHDLExample.Bin2Gray

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signals._

trait Bin2Gray extends ScalaHDL {

  val width = 5

  defMod.bin2gray('g, 'b) {
    async {
      for (i <- 0 until width)
        'g(i) := 'b(i + 1) ^ 'b(i)
      'g(width) := 'b(width)
    }
  }
}

object Main extends Bin2Gray {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val g = unsigned(0, width + 1)
    val b = unsigned(0, width + 1)

    println(convert('bin2gray, g, b))
  }
}
