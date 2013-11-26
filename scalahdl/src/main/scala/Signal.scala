import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName

import ScalaHDL.Simulation.Waiter
import ScalaHDL.Core.NotEnoughBitsException

package ScalaHDL.Core.DataType {

  abstract sealed class HDLDataType

  private object Signal {
    def getBits(value: Int): Int = {
      // Some Faster Method?
      value.toBinaryString.size
    }
  }

  class Signal(val name: String, var _value: Int, _bits: Int)
      extends HDLDataType {
    var eventWaiters: List[Waiter] = List()
    var posedgeWaiters: List[Waiter] = List()
    var negedgeWaiters: List[Waiter] = List()

    def value = _value
    def bits = _bits

    var next: Signal = this

    if (Signal.getBits(_value) > _bits) {
      throw new NotEnoughBitsException(
        name, _value, Signal.getBits(_value), _bits)
    }

    def this(name: String, _value: Int) {
      this(name, _value, Signal.getBits(_value))
    }

    def addWaiter(w: Waiter, v: Int) {
      if (v == 1) posedgeWaiters = w :: posedgeWaiters
      else negedgeWaiters = w :: negedgeWaiters
    }

    def update(): List[Waiter] = {
      var lst = eventWaiters
      // TODO: bits, etc.
      if (_value < next.value)
        lst = posedgeWaiters ::: lst
      else if (value > next.value)
        lst = negedgeWaiters ::: lst
      _value = next.value
      lst
    }

    override def equals(another: Any): Boolean = another match {
      case x: Signal =>
        // only value, every field, or hashcode?
        _value == x.value && _bits == x.bits && name == x.name
      case x: Int =>
        _value == x
      case _ => throw new RuntimeException(
        "comparing Signal with unsupported data type")
    }

    override def toString(): String =
      "Signal %s(value = %d, bits = %d)".format(name, _value, _bits)
  }

  object SignalMaker {
    def signal(value: Int): Signal =
      macro makeSignalWithName1

    def signal(value: Int, bits: Int): Signal =
      macro makeSignalWithName2

    def mkSignal1(name: String, value: Int): Signal =
      new Signal(name, value)

    def makeSignalWithName1(c: Context)(value: c.Expr[Int]) =
      makeCallWithName(c, "SignalMaker.mkSignal1")

    def mkSignal2(name: String, value: Int, bits: Int): Signal =
      new Signal(name, value, bits)

    def makeSignalWithName2(c: Context)(value: c.Expr[Int], bits: c.Expr[Int]) =
      makeCallWithName(c, "SignalMaker.mkSignal2")
  }
}
