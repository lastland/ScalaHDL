import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName

import ScalaHDL.Simulation.Waiter
import ScalaHDL.Core.NotEnoughBitsException

package ScalaHDL.Core.DataType {

  object Edge extends Enumeration {
    type Edge = Value
    val posedge, negedge = Value
  }
  import Edge._

  private object Unsigned {
    def getSize(value: Int): Int = {
      value.abs.toBinaryString.size
    }
  }

  private object Signed {
    def getSize(value: Int): Int = {
      val r = value.abs.toBinaryString.size
      if (value != 0)
        r + 1
      else r
    }
  }

  abstract sealed class Signal(var name: String, var _value: Int, _bits: Int) {
    var eventWaiters: List[Waiter] = List()
    var posedgeWaiters: List[Waiter] = List()
    var negedgeWaiters: List[Waiter] = List()

    protected var next: Int = _value

    def value = _value
    def size = _bits

    checkValid()

    def setNext(n: Int) {
      next = n
    }

    def addWaiter(w: Waiter) {
      eventWaiters = w :: eventWaiters
    }

    def addWaiter(w: Waiter, v: Edge) {
      import Edge._
      if (v == posedge) posedgeWaiters = w :: posedgeWaiters
      else negedgeWaiters = w :: negedgeWaiters
    }

    def update(): List[Waiter] = {
      var lst = eventWaiters
      checkValid()
      if (_value < next)
        lst = posedgeWaiters ::: lst
      else if (value > next)
        lst = negedgeWaiters ::: lst
      _value = next
      lst
    }

    def checkValid(): Unit

    def opposite(): Signal

    def +(other: Signal): Signal
    def -(other: Signal): Signal
    def *(other: Signal): Signal
    def /(other: Signal): Signal

    def <(other: Signal): Boolean =
      value < other.value
    def <=(other: Signal): Boolean =
      value <= other.value
    override def equals(other: Any): Boolean = other match {
      case other: Signal =>
        (value == other.value && name == other.name && size == other.size) ||
        ((name == " " || other.name == " ") && value == other.value)
      case other: Int =>
        value == other
      case _ =>
        throw new RuntimeException("Camparing signal with unsupported data type!")
    }
    def >(other: Signal): Boolean =
      value > other.value
    def >=(other: Signal): Boolean =
      value >= other.value
  }

  class Unsigned(override var name: String, override var _value: Int, _bits: Int)
      extends Signal(name, _value, _bits) {

    def this(name: String, _value: Int) {
      this(name, _value, Unsigned.getSize(_value))
    }

    override def setNext(n: Int) {
      if (n >= 0) {
        next = n % math.pow(2, _bits).toInt
      } else {
        next = math.pow(2, _bits).toInt + n
      }
    }

    override def checkValid() {
      if (_value < 0) {
        throw new IllegalArgumentException("the value cannot be less than 0")
      }
      if (Unsigned.getSize(_value) > _bits) {
        throw new NotEnoughBitsException(
          name, _value, Unsigned.getSize(_value), _bits)
      }
    }

    override def opposite() =
      new Unsigned("", (1 << _bits) - 1 - _value, _bits)

    override def +(other: Signal) = {
      val res = _value + other.value
      if (res >= 0) {
        new Unsigned("", res)
      } else {
        new Signed("", res)
      }
    }

    override def -(other: Signal) = {
      val res = _value - other.value
      if (res >= 0) {
        new Unsigned("", res)
      } else {
        new Signed("", res)
      }
    }

    override def *(other: Signal) = {
      val res = _value * other.value
      if (res >= 0) {
        new Unsigned("", res)
      } else {
        new Signed("", res)
      }
    }

    override def /(other: Signal) = {
      val res = _value / other.value
      if (res >= 0) {
        new Unsigned("", res)
      } else {
        new Signed("", res)
      }
    }

    override def toString(): String =
      "Unsigned %s(value = %d, bits = %d)".format(name, _value, _bits)
  }

  class Bool(override var name: String, override var _value: Int)
      extends Unsigned(name, _value, 1) {
    override def setNext(n: Int) {
      next = n % 2
    }

    override def checkValid() {
      if (_value > 1 || value < 0) {
        throw new NotEnoughBitsException(
          name, _value, Unsigned.getSize(_value), 1)
      }
    }

    override def opposite() =
      new Bool("", 1 - _value)

    override def toString(): String =
      "Bool %s(value = %d)".format(name, _value)
  }

  class Signed(override var name: String, override var _value: Int, _bits: Int)
      extends Unsigned(name, _value, _bits) {

    override def setNext(n: Int) {
      next = n % math.pow(2, _bits - 1).toInt
    }

    override def checkValid() {
      if (Signed.getSize(_value) > _bits)
        throw new NotEnoughBitsException(
          name, _value, Signed.getSize(_value), _bits)
    }

    def this(name: String, _value: Int) {
      this(name, _value, Signed.getSize(_value))
    }

    override def opposite() =
      new Signed("", -value, _bits)

    override def +(other: Signal) =
      new Signed("", _value + other.value)

    override def -(other: Signal) =
      new Signed("", _value - other.value)

    override def *(other: Signal) =
      new Signed("", _value * other.value)

    override def /(other: Signal) =
      new Signed("", _value / other.value)

    override def toString(): String =
      "Signed %s(value = %d, bits = %d)".format(name, _value, _bits)
  }

  object Signals {
    def unsigned(value: Int): Unsigned =
      macro makeUnsignedWithName1

    def unsigned(value: Int, bits: Int): Unsigned =
      macro makeUnsignedWithName2

    def mkUnsigned1(name: String, value: Int): Unsigned =
      new Unsigned(name, value)

    def makeUnsignedWithName1(c: Context)(value: c.Expr[Int]) =
      makeCallWithName(c, "mkUnsigned1")

    def mkUnsigned2(name: String, value: Int, bits: Int): Unsigned =
      new Unsigned(name, value, bits)

    def makeUnsignedWithName2(c: Context)(value: c.Expr[Int], bits: c.Expr[Int]) =
      makeCallWithName(c, "mkUnsigned2")

    def signed(value: Int): Signed =
      macro makeSignedWithName1

    def signed(value: Int, bits: Int): Signed =
      macro makeSignedWithName2

    def mkSigned1(name: String, value: Int): Signed =
      new Signed(name, value)

    def makeSignedWithName1(c: Context)(value: c.Expr[Int]) =
      makeCallWithName(c, "mkSigned1")

    def mkSigned2(name: String, value: Int, bits: Int): Signed =
      new Signed(name, value, bits)

    def makeSignedWithName2(c: Context)(value: c.Expr[Int], bits: c.Expr[Int]) =
      makeCallWithName(c, "mkSigned2")

    def bool(value: Int): Bool =
      macro makeBoolWithName

    def mkBool(name: String, value: Int): Bool =
      new Bool(name, value)

    def makeBoolWithName(c: Context)(value: c.Expr[Int]) =
      makeCallWithName(c, "mkBool")
  }
}
