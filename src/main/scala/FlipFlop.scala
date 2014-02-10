package ScalaHDLExample.FlipFlop

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._

trait FlipFlop extends ScalaHDL {
  defMod.FlipFlop('d, 'q, 'clk) {
    sync(1).logic {
      'q := 'd
    }
  }
}

object Main extends FlipFlop {
  def main(args: Array[String]) {
    val q = bool(0)
    val d = bool(1)
    val clk = bool(0)
    println(convert('FlipFlop, d, q, clk))
  }
}
