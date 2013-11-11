import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Simulation.Waiter

package ScalaHDL.Core.DataType {

  abstract sealed class HDLDataType

  class Signal(val value: Int, val bits: Int) extends HDLDataType {
    var eventWaiters: List[Waiter] = List()
    var posedgeWaiters: List[Waiter] = List()
    var negedgeWaiters: List[Waiter] = List()

    def addWaiter(w: Waiter, v: Int) {
      if (v == 1) posedgeWaiters = w :: posedgeWaiters
      else negedgeWaiters = w :: negedgeWaiters
    }

    def update() {
      // TODO: body
      println("updating!")
    }
  }
}
