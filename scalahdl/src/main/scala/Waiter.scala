package ScalaHDL.Simulation
import ScalaHDL.Core._
import ScalaHDL.Core.DataType._
import ScalaHDL.Helpers.CoroutineHelper._

import scala.collection.mutable.HashMap

class Waiter(stmts: Seq[HDLObject], val sigMap: HashMap[Symbol, Signal]) {
  val iter =
    iterator[List[Waiter]] {
      var senslist: List[Waiter] = List()
      while (true) {
        yld(senslist)
        for (stmt <- stmts) stmt.exec(sigMap)
      }
    }

  def next() = iter.next
}

class SyncWaiter(stmts: Seq[HDLObject], override val sigMap: HashMap[Symbol, Signal])
    extends Waiter(stmts, sigMap)

class AsyncWaiter(stmts: Seq[HDLObject], override val sigMap: HashMap[Symbol, Signal])
    extends Waiter(stmts, sigMap)

class DelayWaiter(stmts: Seq[HDLObject], override val sigMap: HashMap[Symbol, Signal], t: Int)
    extends Waiter(stmts, sigMap) {
  override val iter =
    iterator[List[Waiter]] {
      var senslist: List[Waiter] = List(this)
      while (true) {
        yld(senslist)
        for (stmt <- stmts) stmt.exec(sigMap)
      }
    }

  override def next() = iter.next

  def time = t
}
