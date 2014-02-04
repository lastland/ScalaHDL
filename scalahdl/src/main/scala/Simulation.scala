package ScalaHDL.Simulation

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.module
import ScalaHDL.Core._sync
import ScalaHDL.Core._async
import ScalaHDL.Core._delay
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.HDLCondBlock
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
          val sig = hdl.modules(mod.name).sigMap(param)
          log("$var reg %d %s %s $end".format(sig.size, nameMap(sig), param.name))
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
      if (file != null && writer != null && nameMap.contains(sig))
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
    hdl.sigs = Set()
    var lst: List[Waiter] = List()
    for (mod <- mods) {
      mod.extract(hdl)
      val name = mod.name
      val sigs = mod.sigs
      val hdlmod = hdl.modules(name)
      val params = hdlmod.params
      val stmts = hdlmod.content

      for (sig <- sigs) hdl.sigs = hdl.sigs + sig

      val param_sig = params.zip(sigs).toMap
      for (e <- param_sig) hdlmod.sigMap += e

      for (stmt <- hdlmod.content) {
        stmt match {
          case b: HDLCondBlock =>
            b.cond match {
              case c: _sync =>
                val w = new SyncWaiter(b.content, hdlmod.sigMap)
                param_sig(c.cond.ident.name).addWaiter(w, c.cond.edge)
                lst = w :: lst
              case c: _delay =>
                val w = new DelayWaiter(b.content, hdlmod.sigMap, c.time)
                lst = w :: lst
              // TODO: others
              case c: _async =>
                val w = new AsyncWaiter(b.content, hdlmod.sigMap)
                for (sig <- b.senslist) {
                  hdlmod.sigMap(sig).addWaiter(w)
                }
                schedule(0, w)
                lst = w :: lst
              case _ => throw new RuntimeException("Not supported")
            }
          //TODO: others
          case _ => throw new RuntimeException("Not supported")
        }
      }
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
      hdl.siglist.clear()
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
        if (futureEvents.isEmpty) return waiters
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

    hdl.siglist.clear()
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
}
