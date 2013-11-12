import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Simulation.Waiter

package ScalaHDL.Core.DataType {

  abstract sealed class HDLDataType

  class Signal(var value: Int, val bits: Int) extends HDLDataType {
    var eventWaiters: List[Waiter] = List()
    var posedgeWaiters: List[Waiter] = List()
    var negedgeWaiters: List[Waiter] = List()

    var next: Signal = this

    def addWaiter(w: Waiter, v: Int) {
      if (v == 1) posedgeWaiters = w :: posedgeWaiters
      else negedgeWaiters = w :: negedgeWaiters
    }

    def update(): List[Waiter] = {
      var lst = eventWaiters
      println("updating!")
      // TODO: bits, etc.
      if (value < next.value)
        lst = posedgeWaiters ::: lst
      else if (value > next.value)
        lst = negedgeWaiters ::: lst
      value = next.value
      lst
    }
  }
}
