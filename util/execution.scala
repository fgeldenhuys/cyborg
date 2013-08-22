package cyborg.util

import cyborg.Context
import android.os.Handler
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.{ScheduledThreadPoolExecutor, ScheduledFuture, TimeUnit, ScheduledExecutorService}
import cyborg.Log._

object execution {
  implicit def fun2runnable(f: => Any): Runnable = new Runnable { def run() { f } }

  def runnable(f: => Unit): Runnable =
    new Runnable {
      def run() { f }
    }

  def runOnMainLooper(f: => Unit)(implicit context: Context) {
    (new Handler(context.getMainLooper)).post(f)
  }

  class ScheduledExecutionContext(corePoolSize: Int = 1)
    extends ScheduledThreadPoolExecutor(corePoolSize)
    with ExecutionContext {
    val ec = ExecutionContext.fromExecutorService(this)
    def reportFailure(t: Throwable) { ec.reportFailure(t) }
  }

  case class CancelledExecution(message: String) extends Exception(message)

  class ExecuteWrapper[T](f: () => T)(implicit ses: ScheduledExecutorService) {
    def within(d: Duration): Future[T] = {
      val p = promise[T]()
      var scheduledCancel: Option[ScheduledFuture[_]] = None
      val sf = ses.schedule({
        val result = f()
        scheduledCancel.map(_.cancel(false))
        p success result
      }, 0, TimeUnit.SECONDS)
      scheduledCancel = Some(ses.schedule({
        if (sf.cancel(true))
          p failure CancelledExecution("Timeout reached, function cancelled")
        else {
          $w("Failed to cancel execution, but failing the future. The thread might be blocked forever.")
          p failure CancelledExecution("Timeout reached, failed to cancel function")
        }
      }, d.toSeconds, TimeUnit.SECONDS))
      p.future
    }
  }

  def execute[T](f: => T)(implicit ses: ScheduledExecutorService) = new ExecuteWrapper[T](() => f)
}
