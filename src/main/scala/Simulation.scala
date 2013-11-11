package ScalaHDL.Simulation

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.module
import ScalaHDL.Core._sync
import ScalaHDL.Core._delay
import ScalaHDL.Core.DataType._
import ScalaHDL.Helpers.CoroutineHelper._

import scala.collection.mutable.PriorityQueue

class Simulator(hdl: ScalaHDL){
  private var futureEvents: PriorityQueue[(Int, Waiter)] =
    new PriorityQueue[(Int, Waiter)]()(Ordering[(Int)].on(x => x._1))

  private def wire(mods: Seq[module]) {
    for (mod <- mods) {
      val name = mod.name
      val sigs = mod.sigs
      val hdlmod = hdl.modules(name)
      val params = hdlmod.params
      val cond = hdl.moduleConds(name)

      val param_sig = params.zip(sigs).toMap
      cond match {
        // TODO: the waiter wrap
        case x: _sync => param_sig(x.symbol).addWaiter(
          new SyncWaiter(hdl.moduleStmts(name).reverse), x.cond)
        case _ => ()
      }
    }
  }

  def schedule(time: Int, w: Waiter) {
    futureEvents += ((time, w))
  }

  def simulate(mods: module*) {
    println("start wiring!")
    wire(mods)
    println("end wiring!")
    var waiters: List[Waiter] = List()
    var sigs: List[Signal] = List()

    val f = () => {
      while (true) {
        for (sig <- sigs) sig.update()

        for (waiter <- waiters) {
          waiter.next()
        }

        val spans = futureEvents.span(_._1 == futureEvents.head._1)
        val events = spans._1
        futureEvents = spans._2
        if (events.isEmpty) return
        waiters = events.map(_._2).toList
      }
    }
    f()
  }
}
