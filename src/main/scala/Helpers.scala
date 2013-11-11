package ScalaHDL.Helpers

// From: http://stackoverflow.com/questions/2201882/implementing-yield-yield-return-using-scala-continuations

import scala.util.continuations._

object CoroutineHelper {
  sealed trait Iteration[+R]
  case class Yield[+R](result: R, next: () => Iteration[R]) extends Iteration[R]
  case object Done extends Iteration[Nothing]

  def trampoline[R](body: => Iteration[R]): Iterator[R] = {
    def loop(thunk: () => Iteration[R]): Stream[R] = thunk.apply match {
      case Yield(res, next) => Stream.cons(res, loop(next))
      case Done => Stream.empty
    }
    loop(() => body).iterator
  }

  def iterator[R](body: => Unit @cps[Iteration[R]]): Iterator[R] =
    trampoline {
      reset[Iteration[R], Iteration[R]] { body; Done }
    }

  def yld[R](result: R): Unit @cps[Iteration[R]] =
    shift((k: Unit => Iteration[R]) => Yield(result, () => k(())))
}
