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

    var next: Int = _value

    def value = _value
    def size = _bits

    checkValid()

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
    def equals(other: Signal): Boolean =
      value == other.value
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
      val s = List(size, other.size, Unsigned.getSize(value + other.value)).max
      other match {
        case x: Unsigned => new Unsigned("", _value + other.value, s)
        case _ => throw new RuntimeException("Not supported yet!") // TODO: support signed
      }
    }

    override def -(other: Signal) = {
      val s = List(size, other.size).max
      other match {
        case x: Unsigned => new Unsigned("", _value - other.value, s)
        case _ => throw new RuntimeException("Not supported yet!") // TODO: support signed
      }
    }

    override def *(other: Signal) = {
      val s = List(size, other.size, Unsigned.getSize(value * other.value)).max
        other match {
          case x: Unsigned => new Unsigned("", _value * other.value, s)
          case _ => throw new RuntimeException("Not supported yet!") // TODO: support signed
        }
    }

    override def /(other: Signal) = {
      val s = List(size, other.size).max
      other match {
        // don't need to report / 0 myself
        case x: Unsigned => new Unsigned("", _value / other.value, s)
        case _ => throw new RuntimeException("Not supported yet!") // TODO: support signed
      }
    }

    override def equals(another: Any): Boolean = another match {
      case x: Unsigned =>
        // only value, every field, or hashcode?
        _value == x.value && _bits == x.size && name == x.name
      case x: Int =>
        _value == x
      case _ => throw new RuntimeException(
        "comparing Unsigned with unsupported data type")
    }

    override def toString(): String =
      "Unsigned %s(value = %d, bits = %d)".format(name, _value, _bits)
  }

  class Bool(override var name: String, override var _value: Int)
      extends Unsigned(name, _value, 1) {
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

    override def +(other: Signal) = {
      val s = List(size, other.size, Signed.getSize(value + other.value)).max
      new Signed("", _value + other.value, s)
    }

    override def -(other: Signal) = {
      val s = List(size, other.size).max
      new Signed("", _value - other.value, s)
    }

    override def *(other: Signal) = {
      val s = List(size, other.size, Signed.getSize(value * other.value)).max
      new Signed("", _value * other.value, s)
    }

    override def /(other: Signal) = {
      val s = List(size, other.size).max
      new Signed("", _value / other.value, s)
    }

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
