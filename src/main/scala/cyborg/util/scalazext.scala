package cyborg.util

import scalaz._, Scalaz._
import scalaz.concurrent.Promise

object scalazext {

  object CyborgEitherObjectExt {
    def fromTryCatchNull[A](a: => A): Throwable \/ A = \/.fromTryCatchNonFatal {
      a match {
        case null => throw new NullPointerException()
        case b => b
      }
    }
  }
  implicit def CyborgEitherObjectExtImplicit(x: \/.type) = CyborgEitherObjectExt

  implicit class CyborgThrowableEitherExt[A](val either: Throwable \/ A) extends AnyVal {
    def throwLeft: A = either.fold[A](throw _, identity)
    def getOrThrow = either.fold[A](t => {
      t.printStackTrace
      throw t
    }, identity)
    def getOrLogElse[AA >: A](x: => AA): AA = either.fold({ t =>
      cyborg.Log.$e(t.getMessage)
      t.printStackTrace()
      x
    }, identity)
    def toOptionLog: Option[A] = either.fold({ t =>
      cyborg.Log.$e(t.getMessage)
      t.printStackTrace()
      None
    }, x => Some(x))
    def logLeft: Throwable \/ A = {
      either.leftMap(_.printStackTrace())
      either
    }
  }
}
