package ScalaHDLExample.Matrix.Mult

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signals._

trait Mult extends ScalaHDL {
  defMod.mult('clk, 'rst,
    'a00, 'a01, 'a02, 'a03,
    'a10, 'a11, 'a12, 'a13,
    'a20, 'a21, 'a22, 'a23,
    'b00, 'b01, 'b02,
    'b10, 'b11, 'b12,
    'b20, 'b21, 'b22,
    'b30, 'b31, 'b32,
    'c00, 'c01, 'c02,
    'c10, 'c11, 'c12,
    'c20, 'c21, 'c22) {

    val width_a = 4
    val height_a = 3
    val width_b = 3
    val height_b = 4

    val a = Array(
      Array('a00, 'a01, 'a02, 'a03),
      Array('a10, 'a11, 'a12, 'a13),
      Array('a20, 'a21, 'a22, 'a23)).map(_.map(toHDLType))
    val b = Array(
      Array('b00, 'b01, 'b02),
      Array('b10, 'b11, 'b12),
      Array('b20, 'b21, 'b22),
      Array('b30, 'b31, 'b32)).map(_.map(toHDLType))
    val c = Array(
      Array('c00, 'c01, 'c02),
      Array('c10, 'c11, 'c12),
      Array('c20, 'c21, 'c22)).map(_.map(toHDLType))

    sync(1).mult {
      when ('rst is 1) {
        for (i <- 0 until height_a) {
          for (j <- 0 until width_a) {
            a(i)(j) := 0
          }
        }
        for (i <- 0 until height_b) {
          for (j <- 0 until width_b) {
            b(i)(j) := 0
          }
        }
        for (i <- 0 until height_a) {
          for (j <- 0 until width_b) {
            c(i)(j) := 0
          }
        }
      } .otherwise {
        for (i <- 0 until height_a) {
          for (j <- 0 until width_b) {
            val t = (1 until height_b).map((k) => a(i)(k) * b(k)(j)).foldLeft(
              a(i)(0) * b(0)(j))(_ + _)
            c(i)(j) := t
          }
        }
      }
    }

  }
}

object Main extends Mult {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)

    val a00 = unsigned(0, 5)
    val a01 = unsigned(0, 5)
    val a02 = unsigned(0, 5)
    val a03 = unsigned(0, 5)
    val a10 = unsigned(0, 5)
    val a11 = unsigned(0, 5)
    val a12 = unsigned(0, 5)
    val a13 = unsigned(0, 5)
    val a20 = unsigned(0, 5)
    val a21 = unsigned(0, 5)
    val a22 = unsigned(0, 5)
    val a23 = unsigned(0, 5)

    val b00 = unsigned(0, 5)
    val b01 = unsigned(0, 5)
    val b02 = unsigned(0, 5)
    val b10 = unsigned(0, 5)
    val b11 = unsigned(0, 5)
    val b12 = unsigned(0, 5)
    val b20 = unsigned(0, 5)
    val b21 = unsigned(0, 5)
    val b22 = unsigned(0, 5)
    val b30 = unsigned(0, 5)
    val b31 = unsigned(0, 5)
    val b32 = unsigned(0, 5)

    val c00 = unsigned(0, 5)
    val c01 = unsigned(0, 5)
    val c02 = unsigned(0, 5)
    val c10 = unsigned(0, 5)
    val c11 = unsigned(0, 5)
    val c12 = unsigned(0, 5)
    val c20 = unsigned(0, 5)
    val c21 = unsigned(0, 5)
    val c22 = unsigned(0, 5)

    println(convert('mult, clk, rst,
      a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, a22, a23,
      b00, b01, b02, b10, b11, b12, b20, b21, b22, b30, b31, b32,
      c00, c01, c02, c10, c11, c12, c20, c21, c22))
  }
}
