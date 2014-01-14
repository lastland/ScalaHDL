package ScalaHDL.Simulation

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.module
import ScalaHDL.Core._sync
import ScalaHDL.Core._delay
import ScalaHDL.Core.DataType._
import ScalaHDL.Helpers.CoroutineHelper._

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.util.Date
import java.util.Locale
import java.text.SimpleDateFormat

import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue


class Simulator(hdl: ScalaHDL, mods: Seq[module]){
/*
  object trace {
    var nameMap: Map[Signal, String] = Map[Signal, String]()
    var tracing: Boolean = false
    var file: File = null
    var writer: BufferedWriter = null

    def start(fileName: String) {
      nameMap = hdl.sigs.zip((0 until hdl.sigs.size).map("N" + _.toString)).toMap

      file = new File(fileName)
      writer = new BufferedWriter(new FileWriter(file))

      log(List(
        "$date",
        new SimpleDateFormat("    EEE MMM dd HH:mm:ss yyyy", Locale.UK).format(
          new Date()
        ),
        "$end",
        "$version",
        "    ScalaHDL 0.0.1",
        "$end",
        "$timescale",
        "    1ns",
        "$end"
      ).mkString("\n"))

      log("\n$scope module main $end")
      for (kv <- nameMap)
        log("$var reg %d %s %s $end".format(kv._1.size, kv._2, kv._1.name))
      for (mod <- mods)
      {
        log("$scope module %s $end".format(mod.name.name))
        val params = hdl.modules(mod.name).params
        for (param <- params) {
          val sig = hdl.moduleSigMap(mod.name)(param)
          log("$var reg %d %s %s $end".format(sig.bits, nameMap(sig), param.name))
        }
        log("$upscope $end")
      }
      log("$upscope $end")
      log("\n$enddefinitions $end")
      log("$dumpvars")
      for (sig <- hdl.sigs) logNew(sig)
      log("$end")
    }

    def logNew(sig: Signal) {
      if (file != null && writer != null)
        log("b%s %s".format(sig.value.toBinaryString, nameMap(sig)))
    }

    def stop() {
      if (file != null && writer != null) {
        writer.flush()
        writer.close()
        file = null
        writer = null
      }
    }

    def log(s: String) {
      if (file != null && writer != null) {
        writer.write(s)
        writer.newLine()
      }
    }
  }

  private var futureEvents: PriorityQueue[(Int, Waiter)] =
    new PriorityQueue[(Int, Waiter)]()(Ordering[(Int)].on(x => -x._1))
  private var startRunning: Boolean = false
  private var nexttime = 0
  private var now = 0
  private var waiters: List[Waiter] = List()

  private def getFutureEvents = futureEvents
  private def getNow = now
  def getRunningState = startRunning

  private def wire(mods: Seq[module]): List[Waiter] = {
    hdl.moduleSigMap.clear()
    hdl.sigs = Set()
    var lst: List[Waiter] = List()
    for (mod <- mods) {
      val name = mod.name
      val sigs = mod.sigs
      val hdlmod = hdl.modules(name)
      val params = hdlmod.params
      val cond = hdl.moduleConds(name)
      val stmts = hdl.moduleStmts(name).reverse

      for (sig <- sigs) hdl.sigs = hdl.sigs + sig

      val param_sig = params.zip(sigs).toMap
      cond match {
        // TODO: the waiter wrap
        case x: _sync =>
          val w = new SyncWaiter(stmts, param_sig)
          // TODO: to be rewritten
          param_sig(x.cond.ident.name).addWaiter(w, 0)
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

  private def exec(maxTime: Int, wl: List[Waiter]): List[Waiter] = {
    var waiters = wl
    if (maxTime == 0) return waiters
    while (true) {
      for (sig <- hdl.siglist) {
        waiters = sig.update() ::: waiters
        trace.logNew(sig)
      }
      hdl.siglist = List()
      for (waiter <- waiters) {
        val wl = waiter.next()
        wl.foreach(w => w match {
          case x: DelayWaiter =>
            schedule(nexttime + x.time, x)
          case _ => ()
        })
      }
      now = nexttime
      waiters = List()

      if (hdl.siglist.isEmpty) {
        val spans = futureEvents.span(_._1 == futureEvents.head._1)
        val events = spans._1
        futureEvents = spans._2
        nexttime = events.head._1
        waiters = events.map(_._2).toList
        trace.log("#" + nexttime)
        if (nexttime > maxTime) return waiters
        if (events.isEmpty) return waiters
      }
    }
    waiters
  }

  def simulate(maxTime: Int, fileName: String = "") {
    if (startRunning)
      throw SimulatorException("Simulator is already running!")
    waiters = wire(mods)
    nexttime = 0
    startRunning = true
    if (fileName != "")
      trace.start(fileName)

    hdl.siglist = List()
    waiters = exec(maxTime, waiters)
  }

  def continue(maxTime: Int) {
    if (!startRunning)
      throw SimulatorException("Simulator has not been started!")
    waiters = exec(now + maxTime, waiters)
  }

  def stop() {
    if (!startRunning)
      throw SimulatorException("Simulator has not been started!")
    startRunning = false
    now = 0
    nexttime = 0
    waiters = List()
    trace.stop()
  }
 */
}
