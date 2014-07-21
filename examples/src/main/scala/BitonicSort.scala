package ScalaHDLExample.BitonicSort

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.HDLType
import ScalaHDL.Core.DataType.Signals._

trait BitonicSort extends ScalaHDL {
  val ASC = 0
  val DES = 1

  defMod.sort8('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7,
    'z0, 'z1, 'z2, 'z3, 'z4, 'z5, 'z6, 'z7) {
    val a = List('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7).map(toHDLType)
    val z = List('z0, 'z1, 'z2, 'z3, 'z4, 'z5, 'z6, 'z7).map(toHDLType)

    def compare(a: HDLType, b: HDLType, x: HDLType, y: HDLType, dir: Int) {
      async {
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
    }

    def bitonicMerge(a: Seq[HDLType], z: Seq[HDLType], dir: Int) {
      val n = a.size
      val k = n / 2
      if (n > 1) {
        val t = (for (i <- 0 until n) yield unsigned(0, 4)).map(toHDLType)
        for (i <- 0 until k) {
          compare(a(i), a(i + k), t(i), t(i + k), dir)
        }
        bitonicMerge(t.take(k), z.take(k), dir)
        bitonicMerge(t.drop(k), z.drop(k), dir)
      } else {
        async {
          z.head := a.head
        }
      }
    }

    def bitonicSort(a: Seq[HDLType], z: Seq[HDLType], dir: Int) {
      val n = a.size
      val k = n / 2
      if (n > 1) {
        val t = (for (i <- 0 until n) yield unsigned(0, 4)).map(toHDLType)
        bitonicSort(a.take(k), t.take(k), ASC)
        bitonicSort(a.drop(k), t.drop(k), DES)
        bitonicMerge(t, z, dir)
      } else {
        async {
          z.head := a.head
        }
      }
    }

    bitonicSort(a, z, ASC)
  }
}

object Main extends BitonicSort {
  def main(args: Array[String]) {
    val a0 = unsigned(0, 4)
    val a1 = unsigned(0, 4)
    val a2 = unsigned(0, 4)
    val a3 = unsigned(0, 4)
    val a4 = unsigned(0, 4)
    val a5 = unsigned(0, 4)
    val a6 = unsigned(0, 4)
    val a7 = unsigned(0, 4)

    val z0 = unsigned(0, 4)
    val z1 = unsigned(0, 4)
    val z2 = unsigned(0, 4)
    val z3 = unsigned(0, 4)
    val z4 = unsigned(0, 4)
    val z5 = unsigned(0, 4)
    val z6 = unsigned(0, 4)
    val z7 = unsigned(0, 4)

    println(convert('sort8, a0, a1, a2, a3, a4, a5, a6, a7,
      z0, z1, z2, z3, z4, z5, z6, z7))
  }
}
