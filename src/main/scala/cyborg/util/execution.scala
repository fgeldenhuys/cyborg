package cyborg.util

import cyborg.Context._
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

  class ExecuteWrapper[T](val fun: () => T,
                          val afterwardsFun: Option[() => Any] = None) {
    def now: T = {
      val result = fun()
      afterwardsFun.map(_())
      result
    }

    def within(d: Duration)(implicit sec: ScheduledExecutionContext): Future[T] = {
      val p = promise[T]
      var scheduledCancel: Option[ScheduledFuture[_]] = None
      val sf = sec.schedule({
        val result = fun()
        afterwardsFun.map(_())
        scheduledCancel.map(_.cancel(false))
        p success result
      }, 0, TimeUnit.SECONDS)
      scheduledCancel = Some(sec.schedule({
        if (sf.cancel(true))
          p failure CancelledExecution("Timeout reached, function cancelled")
        else {
          $w("Failed to cancel execution, but failing the future. The thread might be blocked forever.")
          p failure CancelledExecution("Timeout reached, failed to cancel function")
        }
      }, d.toMillis, TimeUnit.MILLISECONDS))
      p.future
    }

    def afterDelayOf(d: Duration)(implicit sec: ScheduledExecutionContext): Future[T] = {
      val p = promise[T]
      sec.schedule({
        val result = fun()
        afterwardsFun.map(_())
        p success result
      }, d.toMillis, TimeUnit.MILLISECONDS)
      p.future
    }

    def repeatedWithDelayOf(d: Duration)(implicit sec: ScheduledExecutionContext): ScheduledFuture[_] = {
      sec.scheduleWithFixedDelay({
        fun()
      }, 0, d.toMillis, TimeUnit.MILLISECONDS)
    }

    def andAfterwards(f: => Any): ExecuteWrapper[T] = {
      new ExecuteWrapper[T](fun, Some(() => f))
    }
  }

  def execute[T](f: => T) = new ExecuteWrapper[T](() => f)
  implicit def implicitExecuteNow[T](ew: ExecuteWrapper[T]): T = ew.now
}
