package ScalaHDL.Simulation

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.module
import ScalaHDL.Core._sync
import ScalaHDL.Core._delay
import ScalaHDL.Core.DataType._
import ScalaHDL.Helpers.CoroutineHelper._

import scala.collection.mutable.PriorityQueue

class Simulator(hdl: ScalaHDL, mods: Seq[module]){
  private var futureEvents: PriorityQueue[(Int, Waiter)] =
    new PriorityQueue[(Int, Waiter)]()(Ordering[(Int)].on(x => -x._1))
  private var startRunning: Boolean = false
  private var runtime = 0
  private var waiters: List[Waiter] = List()

  private def getFutureEvents = futureEvents
  def getRunningState = startRunning

  private def wire(mods: Seq[module]): List[Waiter] = {
    hdl.moduleSigMap.clear()
    var lst: List[Waiter] = List()
    for (mod <- mods) {
      val name = mod.name
      val sigs = mod.sigs
      val hdlmod = hdl.modules(name)
      val params = hdlmod.params
      val cond = hdl.moduleConds(name)
      val stmts = hdl.moduleStmts(name).reverse

      val param_sig = params.zip(sigs).toMap
      cond match {
        // TODO: the waiter wrap
        case x: _sync =>
          val w = new SyncWaiter(stmts, param_sig)
          param_sig(x.symbol).addWaiter(w, x.cond)
          lst = w :: lst
        case x: _delay =>
          val w = new DelayWaiter(stmts, param_sig, x.time)
          lst = w :: lst
        case _ => ()
      }
      hdl.moduleSigMap += (name -> param_sig)
    }
    lst
  }

  def schedule(time: Int, w: Waiter) {
    futureEvents enqueue ((time, w))
  }

  private def exec (maxTime: Int, wl: List[Waiter]): List[Waiter] = {
    var waiters = wl
    while (true) {
      for (sig <- hdl.siglist)
        waiters = sig.update() ::: waiters
      hdl.siglist = List()
      for (waiter <- waiters) {
        val wl = waiter.next()
        wl.foreach(w => w match {
          case x: DelayWaiter =>
            schedule(runtime + x.time, x)
          case _ => ()
        })
      }
      waiters = List()

      if (hdl.siglist.isEmpty) {
        val spans = futureEvents.span(_._1 == futureEvents.head._1)
        val events = spans._1
        futureEvents = spans._2
        runtime = events.head._1
        if (maxTime != 0 && runtime > maxTime) return waiters
        if (events.isEmpty) return waiters
        waiters = events.map(_._2).toList
        println("time = %d".format(runtime))
      }
    }
    waiters
  }

  def simulate(maxTime: Int) {
    // TODO: more appropriate exception
    if (startRunning) throw new RuntimeException
    waiters = wire(mods)
    runtime = 0
    startRunning = true

    hdl.siglist = List()
    println("time = %d".format(runtime))
    waiters = exec(maxTime, waiters)
  }

  def continue(maxTime: Int) {
    // TODO: more appropriate exception
    if (!startRunning) throw new RuntimeException
    waiters = exec(runtime + maxTime, waiters)
  }

  def stop() {
    // TODO: more appropriate exception
    if (!startRunning) throw new RuntimeException
    startRunning = false
    runtime = 0
    waiters = List()
  }
}
