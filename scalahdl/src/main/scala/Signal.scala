import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName

import ScalaHDL.Simulation.Waiter

package ScalaHDL.Core.DataType {

  abstract sealed class HDLDataType

  class Signal(val name: String, var _value: Int, _bits: Int)
      extends HDLDataType {
    var eventWaiters: List[Waiter] = List()
    var posedgeWaiters: List[Waiter] = List()
    var negedgeWaiters: List[Waiter] = List()

    def value = _value
    def bits = _bits

    var next: Signal = this

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
      println("%s is now %d".format(name, _value))
      lst
    }
  }

  object SignalMaker {
    def Signal(value: Int, bits: Int): Signal =
      macro SignalMaker.makeSignalWithName

    def mkSignal(name: String, value: Int, bits: Int): Signal =
      new Signal(name, value, bits)

    def makeSignalWithName(c: Context)(value: c.Expr[Int], bits: c.Expr[Int]) =
      makeCallWithName(c, "SignalMaker.mkSignal")
  }

}
