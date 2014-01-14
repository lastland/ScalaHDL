package ScalaHDLExample.BitonicSort

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.HDLType
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Simulation.Simulator

object Main extends ScalaHDL {
  val ASC = 0
  val DES = 1

  defMod.sort8('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7,
    'z0, 'z1, 'z2, 'z3, 'z4, 'z5, 'z6, 'z7) {
    val a = List('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7).map(toHDLType)
    val z = List('z0, 'z1, 'z2, 'z3, 'z4, 'z5, 'z6, 'z7).map(toHDLType)

    def compare(a: HDLType, b: HDLType, x: HDLType, y: HDLType, dir: Int) {
      if (dir == ASC) {
        when (a > b) {
          x := b
          y := a
        } .otherwise {
          x := a
          y := b
        }
      } else {
        when (a > b) {
          x := a
          y := b
        } .otherwise {
          x := b
          y := a
        }
      }
    }

    def bitonicMerge(a: Seq[HDLType], z: Seq[HDLType], dir: Int) {
      val n = a.size
      val k = n / 2
      if (n > 1) {
        val t = (for (i <- 0 until n) yield bool(0)).map(toHDLType)
        for (i <- 0 until k) {
          compare(a(i), a(i + k), t(i), t(i + k), dir)
        }
        bitonicMerge(t.take(k), z.take(k), dir)
        bitonicMerge(t.drop(k), z.drop(k), dir)
      } else {
        z.head := a.head
      }
    }

    def bitonicSort(a: Seq[HDLType], z: Seq[HDLType], dir: Int) {
      val n = a.size
      val k = n / 2
      if (n > 1) {
        val t = (for (i <- 0 until n) yield bool(0)).map(toHDLType)
        bitonicSort(a.take(k), t.take(k), ASC)
        bitonicSort(a.drop(k), t.drop(k), DES)
        bitonicMerge(t, z, dir)
      } else {
        z.head := a.head
      }
    }

    bitonicSort(a, z, ASC)
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

    println(convert('sort8, a0, a1, a2, a3, a4, a5, a6, a7,
      z0, z1, z2, z3, z4, z5, z6, z7))
  }
}
