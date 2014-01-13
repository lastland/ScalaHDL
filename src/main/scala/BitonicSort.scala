package ScalaHDLExample.BitonicSort

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Simulation.Simulator

object Main extends ScalaHDL {
  val ASC = 0
  val DES = 1

  defMod.compare('a, 'b, 'x, 'y, 'dir) {
    when ('dir == ASC) {
      if ('a > 'b) {
        'x := 'b
        'y := 'a
      }
      else {
        'x := 'a
        'y := 'b
      }
    } elsewhen {
      if ('a > 'b) {
        'x := 'a
        'y := 'b
      }
      else {
        'x := 'b
        'y := 'a
      }
    }
  }

  defMod.sort8('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7,
    'z0, 'z1, 'z2, 'z3, 'z4, 'z5, 'z6, 'z7) {
    'a := List('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
    'z := List('z0, 'z1, 'z2, 'z3, 'z4, 'z5, 'z6, 'z7)

    defFunc.bitonicMerge('a, 'z, 'dir) {
      'n := 'a("size")
      'k := 'n / 2
      if ('n > 1) {
        't := (for (i <- 0 to 'k) yield bool(0))
        for (i <- 0 to 'k) {
          'compare('a(i), 'a(i + 'k), 't(i), 't(i + 'k))
        }
        'bitonicMerge('t("take")(k), z("take")(k), 'dir)
        'bitonicMerge('t("drop")(k), z("drop")(k), 'dir)
      }
      else {
        'z := 'a
      }
    }

    defFunc.bitonicSort('a, 'z, 'dir) {
      'n := 'a("size")
      'k := 'n / 2
      if ('n > 1) {
        't = (for (i <- 0 to 'k) yield bool(0))
        'bitonicSort('a("take")(k), z("take")(k), ASC)
        'bitonicSort('a("drop")(k), z("drop")(k), DES)
        'bitonicMerge('t, 'z, 'dir)
      }
    }
    'bitonicSort('a, 'z, ASC)
  }

  def main(args: Array[String]) {
    val a0 = bool(0)
    val a1 = bool(0)
    val a2 = bool(0)
    val a3 = bool(0)
    val a4 = bool(0)
    val a5 = bool(0)
    val a6 = bool(0)
    val a7 = bool(0)

    val z0 = bool(0)
    val z1 = bool(0)
    val z2 = bool(0)
    val z3 = bool(0)
    val z4 = bool(0)
    val z5 = bool(0)
    val z6 = bool(0)
    val z7 = bool(0)

    convert('sort8, a0, a1, a2, a3, a4, a5, a6, a7,
      z0, z1, z2, z3, z4, z5, z6, z7)
  }
}
