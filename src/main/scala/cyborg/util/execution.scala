package cyborg.util

import android.os.Handler
import cyborg.Context._
import cyborg.Log._
import java.util.concurrent.{ScheduledThreadPoolExecutor, ScheduledFuture => SF, TimeUnit}
import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.duration._

object execution {
  implicit def fun2runnable(f: => Any): Runnable = new Runnable { def run() { f } }

  def systemTime: Long = System.currentTimeMillis()

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

  case class ScheduledFuture[T](future: Future[T], scheduled: SF[_])

  case class CancelledExecution(message: String) extends Exception(message)

  class ExecuteWrapper[T](val fun: () => T,
                          val afterwardsFun: Option[() => Any] = None) {

    def andAfterwards(f: => Any): ExecuteWrapper[T] = {
      new ExecuteWrapper[T](fun, Some(() => f))
    }

    def now: T = {
      val result = fun()
      afterwardsFun.map(_())
      result
    }

    def future(implicit sec: ScheduledExecutionContext): Future[T] = scala.concurrent.future(now)

    // Runs original function, and tries to kill it if it runs longer than d and returns.
    def within(d: Duration)(implicit sec: ScheduledExecutionContext): Future[T] = {
      val p = promise[T]
      var scheduledCancel: Option[SF[_]] = None // To cancel the timeout sentinel
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

    def afterDelayOf(d: Duration)(implicit sec: ScheduledExecutionContext): ScheduledFuture[T] = {
      val p = promise[T]
      val s = sec.schedule({
        val result = fun()
        afterwardsFun.map(_())
        p success result
      }, d.toMillis, TimeUnit.MILLISECONDS)
      ScheduledFuture[T](p.future, s)
    }

    // Run immediately and then repeatedly with delay of d in between executions. The result is lost.
    def repeatedWithDelayOf(d: Duration)(implicit sec: ScheduledExecutionContext): SF[_] = {
      sec.scheduleWithFixedDelay({
        fun()
      }, 0, d.toMillis, TimeUnit.MILLISECONDS)
    }

    // Returns a confirm function. If confirm is called before the timeout the original function runs and
    // returns Some(result). If it is called after timeout, doesn't run and returns None.
    def ifConfirmedWithin(d: Duration)(implicit sec: ScheduledExecutionContext): ConfirmExecution[T] = {
      new ConfirmExecution[T](d, this)
    }
  }

  def execute[T](f: => T) = new ExecuteWrapper[T](() => f)

  def serialExecution[A](list: List[ExecuteWrapper[Future[A]]])
                        (implicit sec: ScheduledExecutionContext): Future[List[A]] = {
    scala.concurrent.future {
      for (task <- list) yield Await.result(task.now, Duration.Inf)
    }
  }

  class ConfirmExecution[T](d: Duration, val wrapped: ExecuteWrapper[T]) {
    var timeout = systemTime + d.toMillis

    def confirm: Option[T] = {
      val current = systemTime
      if (current <= timeout) {
        val result = wrapped.fun()
        wrapped.afterwardsFun.map(_())
        Some(result)
      }
      else
        None
    }

    def confirmOrElse[A](f: => A) = confirm getOrElse f

    def reset(d: Duration) {
      timeout = systemTime + d.toMillis
    }
  }

  class ExecutionTimer(val start: Long, val name: String) {
    var lastCheckpoint: Long = start
    lazy val averageTimes = mutable.HashMap.empty[String, (Int, Long)]

    def apply(): Duration = (systemTime - start).milliseconds

    def checkpoint(message: String): Duration = {
      val now = systemTime
      val t = now - start
      val split = now - lastCheckpoint
      lastCheckpoint = now
      $d(s"$name +$t ($split) $message")
      split.milliseconds
    }

    def averageCheckpoint(message: String): Duration = {
      val now = systemTime
      val split = now - lastCheckpoint
      lastCheckpoint = now
      val (n, t) = averageTimes.get(message) getOrElse (0, 0l)
      averageTimes(message) = (n + 1, t + split)
      split.milliseconds
    }

    def averageReport: String =
      (for ((message, (n, time)) <- averageTimes.toMap) yield
        s"$message \t $time / $n = ${time / n} ms").mkString("\n")

    def silentCheckpoint: Duration = {
      val now = systemTime
      val split = now - lastCheckpoint
      lastCheckpoint = now
      split.milliseconds
    }

  }
  def startTimer(name: String = "TIMER") = new ExecutionTimer(systemTime, name)
}
