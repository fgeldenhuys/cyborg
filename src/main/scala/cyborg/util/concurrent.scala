package cyborg.util

import cyborg.util.duration._
import scala.concurrent._
import scala.util.Try

object concurrent {
  def sleep(duration: Duration): Unit = {
    try {
      Thread.sleep(milliseconds(duration))
    }
    catch {
      case e: InterruptedException => {}
    }
  }

  def resultIfComplete[A](f: Future[A]): Option[A] = {
    f.value match {
      case None => None
      case Some(scala.util.Failure(_)) => None
      case Some(scala.util.Success(a)) => Some(a)
    }
  }

  implicit class FutureCyborgExt[T](val f: Future[T]) extends AnyVal {
    def blockOption(max: Duration): Option[T] = Try(Await.result(f, max.toScalaStd)).toOption
  }

  implicit class SyncVarCyborgExt[A](val sv: SyncVar[A]) extends AnyVal {
    def pop(timeout: Long): Option[A] = Try(sv.take(timeout)).toOption
  }
}