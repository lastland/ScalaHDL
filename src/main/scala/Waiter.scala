package ScalaHDL.Simulation
import ScalaHDL.Core._
import ScalaHDL.Helpers.CoroutineHelper._

class Waiter(stmts: Seq[HDLObject]) {
  val iter =
    iterator[List[Waiter]] {
      var senslist: List[Waiter] = List()
      while (true) {
        yld(senslist)
        for (stmt <- stmts) stmt.exec
      }
    }

  def next() = iter.next
}

class SyncWaiter(stmts: Seq[HDLObject]) extends Waiter(stmts)

class DelayWaiter(stmts: Seq[HDLObject], t: Int) extends Waiter(stmts) {
  override val iter =
    iterator[List[Waiter]] {
      var senslist: List[Waiter] = List(this)
      while (true) {
        yld(senslist)
        for (stmt <- stmts) stmt.exec
      }
    }

  override def next() = iter.next

  def time = t
}
