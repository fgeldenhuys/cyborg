package cyborg.util

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
}
