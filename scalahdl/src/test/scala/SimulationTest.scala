import org.scalatest.Suite
import org.scalatest.PrivateMethodTester
import scala.util.Random
import scala.collection.mutable.PriorityQueue

import ScalaHDL.Simulation._
import ScalaHDL.Core._
import ScalaHDL.Core.DataType._

class SimulationTest extends Suite with PrivateMethodTester {

  object Mod1 extends ScalaHDL {
    sync('clk is 1)
    defMod.logic('d, 'q, 'clk) {
      'q := 'd
    }

    delay(10)
    defMod.clkGen('clk) {
      cycle('clk)
    }

    sync('clk is 0)
    defMod.stimulus('d, 'clk) {
      'd := Random.nextInt(2)
    }
  }

  /*
   * This test is HDL related.
   */
  def testWire() {
    val wire = PrivateMethod[List[Waiter]]('wire)
    val q = new Signal("q", 0, 1)
    val d = new Signal("d", 1, 1)
    val clk = new Signal("clk", 0, 1)
    val mods = List(new module('logic, d, q, clk),
      new module('clkGen, clk),
      new module('stimulus, d, clk)
    )
    val sim = new Simulator(Mod1, mods)
    val lst = sim invokePrivate wire(mods)
    assert(Mod1.sigs.size === 3)
    for (waiter <- lst) {
      for (kv <- waiter.sigMap) {
        kv._1 match {
          case 'q => assert(kv._2 === q)
          case 'd => assert(kv._2 === d)
          case 'clk => assert(kv._2 === clk)
          case _ => assert(false) // not supposed to happen
        }
      }
    }
    // TODO: test with more modules
  }

  def testSimulateAndContinue() {
    val q = new Signal("q", 0)
    val d = new Signal("d", 0)
    val clk = new Signal("clk", 0)
    val sim = new Simulator(Mod1, List(
      new module('logic, d, q, clk),
      new module('clkGen, clk),
      new module('stimulus, d, clk)
    ))

    val getNow = PrivateMethod[Int]('getNow)
    var now = 10
    sim.simulate(10)
    assert((sim invokePrivate getNow()) === 10)
    for (i <- 1 to 100) {
      now += 10
      sim.continue(10)
      assert(clk === new Signal("clk", 0))
      assert((sim invokePrivate getNow()) === now)
      now += 10
      sim.continue(10)
      assert(clk === new Signal("clk", 1))
      assert(q.value === d.value)
      assert((sim invokePrivate getNow()) === now)
    }
    sim.stop()
    // TODO: test with more modules
  }

  def testRestart() {
    val q = new Signal("q", 0)
    val d = new Signal("d", 0)
    val clk = new Signal("clk", 0)
    val sim = new Simulator(Mod1, List(
      new module('logic, d, q, clk),
      new module('clkGen, clk),
      new module('stimulus, d, clk)
    ))
    sim.simulate(1000)
    sim.continue(1000)
    sim.stop()

    val getNow = PrivateMethod[Int]('getNow)
    var now = 10
    sim.simulate(10)
    assert((sim invokePrivate getNow()) === 10)
    for (i <- 1 to 100) {
      now += 10
      sim.continue(10)
      assert(clk === 0)
      assert((sim invokePrivate getNow()) === now)
      now += 10
      sim.continue(10)
      assert(clk === 1)
      assert(q.value === d.value)
      assert((sim invokePrivate getNow()) === now)
    }
    sim.stop()
    // TODO: test with more modules
  }

  def testIllegalUsage() {
    val q = new Signal("q", 0)
    val d = new Signal("d", 0)
    val clk = new Signal("clk", 0)
    val sim = new Simulator(Mod1, List(
      new module('logic, d, q, clk),
      new module('clkGen, clk),
      new module('stimulus, d, clk)
    ))
    intercept[SimulatorException] {
      sim.continue(10)
    }
    intercept[SimulatorException] {
      sim.stop()
    }
    sim.simulate(10)
    intercept[SimulatorException] {
      sim.simulate(10)
    }
    sim.stop()
  }

  /*
   * This test is HDL unrelated.
   */
  def testSchedule() {
    val getWaiter = (t: Int) => (t, new SyncWaiter(List(), Map()))
    val lst = List(
      getWaiter(100),
      getWaiter(200),
      getWaiter(100),
      getWaiter(1000),
      getWaiter(200),
      getWaiter(150),
      getWaiter(100),
      getWaiter(10)
    )
    val expected = lst.sortBy(_._1)

    val sim = new Simulator(Mod1, List())
    for (e <- lst) sim.schedule(e._1, e._2)

    val getFuture = PrivateMethod[PriorityQueue[(Int, Waiter)]]('getFutureEvents)
    val res = sim invokePrivate getFuture()
    assert(res.span(_._1 == res.head._1)._1.head === expected.head)
    var r = res
    var e = expected
    while (!r.isEmpty)
    {
      val (ar, br) = r.span(_._1 == r.head._1)
      val (ae, be) = e.span(_._1 == e.head._1)
      assert(ar.toSet === ae.toSet)
      r = br
      e = be
    }
    // TODO: test with more cases?
  }
}
