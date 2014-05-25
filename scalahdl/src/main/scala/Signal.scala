import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName

import ScalaHDL.Simulation.Waiter
import ScalaHDL.Core.NotEnoughBitsException
import ScalaHDL.Core.BitwiseTypeMismatchException

package ScalaHDL.Core.DataType {

  object Edge extends Enumeration {
    type Edge = Value
    val posedge, negedge = Value
  }
  import Edge._

  private object Signal {
    def mkSignal(l: Signal, r: Signal,
      f: (Int, Int) => Int): Signal = {
      val res = f(l.value, r.value)
      l match {
        case _: Signed => new Signed("", res)
        case _ => r match {
          case _: Signed => new Signed("", res)
          case _ => new Unsigned("", res)
        }
      }
    }
  }

  private object Unsigned {
    def getSize(value: Int): Int = {
      value.abs.toBinaryString.size
    }
  }

  private object Signed {
    def getSize(value: Int): Int = {
      val s = value.toBinaryString
      if (value == -1)
        2
      else if (value < 0)
        ("1" + s.dropWhile(_ == '1')).size
      else
        s.size + 1
    }
  }

  abstract sealed class Signal(var name: String, var _value: Int, _bits: Int) {
    var eventWaiters: List[Waiter] = List()
    var posedgeWaiters: List[Waiter] = List()
    var negedgeWaiters: List[Waiter] = List()

    protected var next: Int = _value

    def value = _value
    def size = _bits

    checkValid(_value)

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
      checkValid(next)
      if (_value < next)
        lst = posedgeWaiters ::: lst
      else if (value > next)
        lst = negedgeWaiters ::: lst
      _value = next
      lst
    }

    def checkValid(num: Int): Unit

    def &(other: Signal): Signal
    def |(other: Signal): Signal
    def ^(other: Signal): Signal

    def &&(other: Signal): Signal =
      throw new RuntimeException("Only support for Bool!")
    def ||(other: Signal): Signal =
      throw new RuntimeException("Only support for Bool!")

    def unary_~(): Signal
    def unary_!(): Signal =
      throw new RuntimeException("Only support for Bool!")

    def +(other: Signal): Signal
    def -(other: Signal): Signal
    def *(other: Signal): Signal
    def /(other: Signal): Signal
    def %(other: Signal): Signal

    def <<(other: Signal): Signal
    def >>(other: Signal): Signal

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

    override def checkValid(num: Int) {
      if (num < 0) {
        throw new IllegalArgumentException("the value cannot be less than 0")
      }
      if (Unsigned.getSize(num) > _bits) {
        throw new NotEnoughBitsException(
          name, num, Unsigned.getSize(num), _bits)
      }
    }

    override def unary_~(): Signal =
      new Unsigned("", (~value & (1 << _bits) - 1), _bits)

    override def +(other: Signal) =
      Signal.mkSignal(this, other, (a, b) => a + b)

    override def -(other: Signal) =
      new Signed("", value - other.value)

    override def *(other: Signal) =
      Signal.mkSignal(this, other, (a, b) => a * b)

    override def /(other: Signal) =
      Signal.mkSignal(this, other, (a, b) => a / b)

    override def %(other: Signal) =
      Signal.mkSignal(this, other, (a, b) => a % b)

    override def &(other: Signal) = other match {
        case u: Unsigned => new Unsigned("", value & other.value)
        case _ => throw new BitwiseTypeMismatchException
      }

    override def |(other: Signal) = other match {
      case u: Unsigned => new Unsigned("", value | other.value)
      case _ => throw new BitwiseTypeMismatchException
    }

    override def ^(other: Signal) = other match {
      case u: Unsigned => new Unsigned("", value ^ other.value)
      case _ => throw new BitwiseTypeMismatchException
    }

    override def <<(other: Signal) =
      new Unsigned("", value << other.value)

    override def >>(other: Signal) =
      new Unsigned("", value >> other.value)

    override def toString(): String =
      "Unsigned %s(value = %d, bits = %d)".format(name, _value, _bits)
  }

  class Bool(override var name: String, override var _value: Int)
      extends Unsigned(name, _value, 1) {

    override def unary_~() =
      new Bool("", 1 - _value)

    override def unary_!() =
      new Bool("", 1 - _value)

    override def &&(other: Signal) = other match {
      case b: Bool => new Bool("", _value & b.value)
      case _ => throw new RuntimeException("only support for Bool!")
    }

    override def ||(other: Signal) = other match {
      case b: Bool => new Bool("", _value | b.value)
      case _ => throw new RuntimeException("only support for Bool!")
    }

    override def &(other: Signal) = other match {
      case u: Bool => new Bool("", value & other.value)
      case _ => throw new BitwiseTypeMismatchException
    }

    override def |(other: Signal) = other match {
      case u: Bool => new Bool("", value | other.value)
      case _ => throw new BitwiseTypeMismatchException
    }

    override def ^(other: Signal) = other match {
      case u: Bool => new Bool("", value ^ other.value)
      case _ => throw new BitwiseTypeMismatchException
    }

    override def toString(): String =
      "Bool %s(value = %d)".format(name, _value)
  }

  class Signed(override var name: String, override var _value: Int, _bits: Int)
      extends Unsigned(name, _value, _bits) {

    override def checkValid(num: Int) {
      if (Signed.getSize(num) > _bits)
        throw new NotEnoughBitsException(
          name, num, Signed.getSize(num), _bits)
    }

    def this(name: String, _value: Int) {
      this(name, _value, Signed.getSize(_value))
    }

    override def unary_~(): Signal =
      new Signed("", ~value, _bits)

    override def /(other: Signal) =
      if (_value < 0)
        Signal.mkSignal(this, other, (a, b) => a / b - 1)
      else
        Signal.mkSignal(this, other, (a, b) => a / b)

    override def %(other: Signal) =
      if (_value < 0 && _value % other.value != 0)
        Signal.mkSignal(this, other, (a, b) => a - (a / b - 1) * b)
      else
        Signal.mkSignal(this, other, (a, b) => a % b)

    // TODO: extend signed bits

    override def &(other: Signal) = other match {
      case u: Signed => new Signed("", value & other.value)
      case _ => throw new BitwiseTypeMismatchException
    }

    override def |(other: Signal) = other match {
      case u: Signed => new Signed("", value | other.value)
      case _ => throw new BitwiseTypeMismatchException
    }

    override def ^(other: Signal) = other match {
      case u: Signed => new Signed("", value ^ other.value)
      case _ => throw new BitwiseTypeMismatchException
    }

    override def <<(other: Signal) =
      new Signed("", value << other.value)

    override def >>(other: Signal) =
      new Signed("", value >> other.value)

    override def toString(): String =
      "Signed %s(value = %d, bits = %d)".format(name, _value, _bits)
  }

  class SignalBit(val sig: Signal, idx: Int)
      extends Bool("", (sig.value >> idx) % 2) {

    override def update(): List[Waiter] = {
      if (_value != next) {
        _value = next
        sig.setNext(sig.value ^ (1 << idx))
      }
      sig.update()
    }

    override def toString(): String =
      "SignalBit " + idx + " of " + sig.toString
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
