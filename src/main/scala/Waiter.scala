package ScalaHDL.Simulation
import ScalaHDL.Core._
import ScalaHDL.Core.DataType._
import ScalaHDL.Helpers.CoroutineHelper._

class Waiter(stmts: Seq[HDLObject], val sigMap: Map[Symbol, Signal]) {
  val iter =
    iterator[List[Waiter]] {
      var senslist: List[Waiter] = List()
      while (true) {
        for (stmt <- stmts) stmt.exec(sigMap)
        yld(senslist)
      }
    }

  def next() = iter.next
}

class SyncWaiter(stmts: Seq[HDLObject], override val sigMap: Map[Symbol, Signal])
    extends Waiter(stmts, sigMap)

class DelayWaiter(stmts: Seq[HDLObject], override val sigMap: Map[Symbol, Signal], t: Int)
    extends Waiter(stmts, sigMap) {
  override val iter =
    iterator[List[Waiter]] {
      var senslist: List[Waiter] = List(this)
      while (true) {
        for (stmt <- stmts) stmt.exec(sigMap)
        yld(senslist)
      }
    }

  override def next() = iter.next

  def time = t
}
