package cyborg.util

import scalaz._

object javahelpers {

  // \/ helpers
  def throwableLeft[A](t: Throwable): Throwable \/ A = -\/(t)
  def throwableRight[A](a: A): Throwable \/ A = \/-(a)
  def either2throw[A](x: Throwable \/ A): A = x.fold({ l => throw l }, identity)
  abstract class ThrowableEitherFold[A, B] {
    def run(either: Throwable \/ A): B = either.fold(left, right)
    def left(t: Throwable): B
    def right(a: A): B
  }
  abstract class ThrowableEitherThrow[A, B] {
    def run(either: Throwable \/ A): B = either.fold({ l => throw l }, { r => right(r).fold({ l => throw l }, identity) })
    def right(a: A): Throwable \/ B
  }

}
