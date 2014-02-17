package ScalaHDL.Test

import scala.collection.mutable.PriorityQueue

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.DataType.Signal
import ScalaHDL.Core.module

trait TestHelper {
  def Simulator(hdl: ScalaHDL, mods: module*) =
    new SimulatorForTest(hdl, mods)

  def module(modName: Symbol, args: Signal*) =
    new module(modName, args:_*)
}


class SimulatorForTest(hdl: ScalaHDL, mods: Seq[module])
    extends ScalaHDL.Simulation.Simulator(hdl, mods) {
  var tasks: PriorityQueue[(Int, () => Unit)] =
    new PriorityQueue[(Int, () => Unit)]()(Ordering[(Int)].on(x => -x._1))

  case class TestStartPoint(time: Int) {
    def to(endTime: Int) = TestDuration(time, endTime)
    def until(endTime: Int) = TestDuration(time, endTime - 1)
  }

  case class TestDuration(startTime: Int, endTime: Int) {
    def every(duration: Int) =  TestSchedule(startTime, endTime, duration)
  }

  case class TestSchedule(startTime: Int, endTime: Int, duration: Int) {
    def run(f: => Unit) {
      (startTime to endTime by duration).foreach { t =>
        tasks enqueue ((t, () => f))
      }
    }
  }

  case class TestTask(s: TestSchedule, f: () => Unit)

  def since(time: Int): TestStartPoint = TestStartPoint(time)

  def test() {
    simulate(0)
    var now = 0
    while (!tasks.isEmpty) {
      val (time, task) = tasks.dequeue
      if (time > now)
        continue(time - now)
      task()
      now = time
    }
    stop()
  }
}
