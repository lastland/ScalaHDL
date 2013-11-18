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
    val sim = new Simulator(Mod1)
    val wire = PrivateMethod[List[Waiter]]('wire)
    val q = new Signal(0, 1)
    val d = new Signal(1, 1)
    val clk = new Signal(0, 1)
    val lst = sim invokePrivate wire(List(
      new module('logic, d, q, clk),
      new module('clkGen, clk),
      new module('stimulus, d, clk)
    ))
    for (waiter <- lst) {
      for (kv <- waiter.sigMap) {
        println(kv)
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

    val sim = new Simulator(Mod1)
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
