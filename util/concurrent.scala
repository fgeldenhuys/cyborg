package cyborg.util

import scala.concurrent._
import scala.concurrent.duration._

object concurrent {
  def awaitResultOption[T](awaitable: Awaitable[T], atMost: Duration): Option[T] =
    try {
      Some(Await.result(awaitable, atMost))
    } catch {
      case e: TimeoutException => None
    }

  def sleep(duration: Duration) {
    try {
      Thread.sleep(duration.toMillis)
    }
    catch {
      case e: InterruptedException => {}
    }
  }
}
