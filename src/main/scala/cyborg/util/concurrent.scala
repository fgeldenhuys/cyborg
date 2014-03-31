package cyborg.util

import cyborg.util.execution.ExecuteWrapper
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Try

object concurrent {
  def sleep(duration: Duration) {
    try {
      Thread.sleep(duration.toMillis)
    }
    catch {
      case e: InterruptedException => {}
    }
  }

  implicit class FutureCyborgExt[T](val f: Future[T]) extends AnyVal {
    def blockOption(max: Duration): Option[T] = Try(Await.result(f, max)).toOption
  }

  implicit class SyncVarCyborgExt[A](val sv: SyncVar[A]) extends AnyVal {
    def pop(timeout: Long): Option[A] = Try(sv.take(timeout)).toOption
  }
}
