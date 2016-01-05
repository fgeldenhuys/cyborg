package cyborg.util

import java.util

import android.os.Handler
import cyborg.Context._
import cyborg.Log._
import java.util.concurrent.{ScheduledFuture => SF, ThreadPoolExecutor, RejectedExecutionHandler, ScheduledThreadPoolExecutor, TimeUnit}
import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.duration._

object execution {
  @deprecated("This is bad") implicit def fun2runnable(f: => Any): Runnable = new Runnable { def run(): Unit = { f } }

  def systemTime: Long = System.currentTimeMillis()

  def runnable(f: => Unit): Runnable =
    new Runnable {
      def run(): Unit = { f }
    }

  def runOnMainLooper(f: => Unit)(implicit context: Context): Unit = {
    (new Handler(context.getMainLooper)).post(f)
  }

  class ScheduledExecutionContext(corePoolSize: Int = 1, val name: String = "THREADPOOL")
    extends ScheduledThreadPoolExecutor(corePoolSize, new RejectedExecutionHandler {
      override def rejectedExecution(r: Runnable, executor: ThreadPoolExecutor): Unit = {
        if (executor.isShutdown) {
          $d(s"$name is discarding task after shutdown")
        }
        else {
          $w(s"$name rejected execution, spawning temporary thread")
          new Thread(r).run()
        }
      }
    })
    //TODO: ExecutionContext is for scala futures, and should eventually be removed
    with ExecutionContext {
    $d(s"Creating thread pool $name")
    prestartAllCoreThreads()

    override def submit[T](task: java.util.concurrent.Callable[T]): java.util.concurrent.Future[T] = {
      synchronized {
        if (getActiveCount == getPoolSize) {
          $i(s"Growing pool $name to core size ${getPoolSize + 1}.")
          setCorePoolSize(getPoolSize + 1)
        }
      }
      super.submit(task)
    }

    def reportFailure(t: Throwable): Unit = {
      $e(s"$name report failure: " + t)
      t.printStackTrace()
    }

    override def shutdown(): Unit = {
      $w(s"$name shutting down")
      super.shutdown()
    }

    override def shutdownNow(): util.List[Runnable] = {
      $w(s"$name shutting down NOW")
      super.shutdownNow()
    }
  }

  object ScheduledExecutionContext {
    def apply(poolSize: Int, name: String) = {
      import cyborg.util.duration._
      new ScheduledExecutionContext(poolSize, name + milliseconds(now).toString.takeRight(5).dropRight(1))
    }
  }

  case class ScheduledFuture[T](future: Future[T], scheduled: SF[_])

  case class CancelledExecution(message: String) extends Exception(message)

  object Tasks {
    import scala.concurrent.{ExecutionContext, Future => SFuture}
    import scala.util.Try
    import _root_.scalaz.\/
    import _root_.scalaz.concurrent.{Task => ZTask}

    def fromScala[A](future: SFuture[A])(implicit ec: ExecutionContext): ZTask[A] =
      ZTask async (handlerConversion andThen future.onComplete)

    private def handlerConversion[A]: ((Throwable \/ A) => Unit) => Try[A] => Unit =
      callback => { t: Try[A] => \/ fromTryCatch t.get } andThen callback
  }

  class ExecutionLock(val atomic: java.util.concurrent.atomic.AtomicBoolean) extends AnyVal {
    // This is NOT BLOCKING. Only executes f if the block has not already been entered, else returns None.
    def apply[A](f: => A): Option[A] = {
      if (atomic.compareAndSet(false, true)) {
        try {
          Some(f)
        }
        finally {
          atomic.set(false)
          None
        }
      }
      else None
    }

    def runOnUiThread(f: => Any)(implicit activity: android.app.Activity, sec: ScheduledExecutionContext): Unit = {
      activity.runOnUiThread(new Runnable {
        override def run(): Unit =
          if (atomic.compareAndSet(false, true)) {
            try {
              f
            }
            finally {
              atomic.set(false)
            }
          }
      })
    }
  }

  object ExecutionLock {
    def apply(): ExecutionLock =
      new ExecutionLock(new java.util.concurrent.atomic.AtomicBoolean(false))
  }

  @deprecated("Use scalaz.concurrent.Task")
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
    def within(d: Duration, message: String = "")(implicit sec: ScheduledExecutionContext): Future[T] = {
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
          p failure CancelledExecution("Timeout reached, function cancelled: " + message)
        else {
          $w(s"Failed to cancel execution for '$message', but failing the future. The thread might be blocked forever.")
          p failure CancelledExecution("Timeout reached, failed to cancel function: " + message)
        }
      }, d.toMillis, TimeUnit.MILLISECONDS))
      p.future
    }

    def runAndAfter(d: Duration)(after: => Any)(implicit sec: ScheduledExecutionContext): T = {
      val sf = sec.schedule({
        after
      }, d.toMillis, TimeUnit.MILLISECONDS)
      try {
        fun()
      }
      finally {
        sf.cancel(true)
      }
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

  @deprecated("Use scalaz.concurrent.Task")
  def execute[T](f: => T) = new ExecuteWrapper[T](() => f)

  @deprecated("Use scalaz.concurrent.Task")
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

    def reset(d: Duration): Unit = {
      timeout = systemTime + d.toMillis
    }
  }

  class ExecutionTimer(val start: Long, val name: String, val silent: Boolean, val disabled: Boolean) {
    import ExecutionTimer._

    val events = mutable.ListBuffer.empty[TimerEvent]

    def lastEventTime: Long = events.lastOption.fold(start)(_.mark)

    def apply(): Duration = (systemTime - start).milliseconds

    private def _check(message: => String, log: Boolean): Duration =
      if (disabled) 0.seconds
      else {
        val now = systemTime
        val t = now - start
        val split = now - lastEventTime
        val event = TimerEvent(now, t, split, message, None)
        events += event
        if (log) $d(event.toString, 2)
        split.milliseconds
      }

    def check(message: => String) = _check(message, !silent)
    def silentCheck(message: => String) = _check(message, log = false)

    def aveCheck(group: => String, message: => String): Duration =
      if (disabled) 0.seconds
      else {
        val now = systemTime
        val t = now - start
        val split = now - lastEventTime
        val event = TimerEvent(now, t, split, message, Some(group))
        events += event
        split.milliseconds
      }

    def groupTotals: List[(String, Int, Long)] =
      if (disabled) List.empty
      else {
        events.foldLeft(List.empty[(String, Int, Long)]) { (acc, event) =>
          event.group.fold (acc) { g =>
            if (acc.exists(_._1 == g)) {
              acc collect {
                case (group, n, time) if group == g => (group, n + 1, time + event.split)
                case other => other
              }
            }
            else (acc ++ List((g, 1, event.split)))
          }
        }
      }

    def averageReport: String =
      if (disabled) "DISABLED"
      else (for ((group, n, time) <- groupTotals)
        yield s"$group \t $time / $n = ${time.toFloat / n.toFloat} ms").mkString("\n")

    def sortedAverageReport: String =
      if (disabled) "DISABLED"
      else {
        groupTotals map { gt =>
          val (group, n, time) = gt
          (time / n, group, n, time)
        } sortWith { (x, y) =>
          x._1 < y._1
        } map { ga =>
          val (ave, group, n, time) = ga
          s"$group \t $time / $n = $ave ms"
        } mkString "\n"
      }

    def fullReport: String =
      if (disabled) "DISABLED" else events mkString "\n"
  }

  object ExecutionTimer {
    //import scala.reflect.macros.{Context => MacroContext}

    // mark is when event happened, t is how long after start of timer, split is how long after previous event
    case class TimerEvent(mark: Long, t: Long, split: Long, message: String, group: Option[String]) {
      override def toString: String = s"+$t\t($split)\t${group.fold("")("["+_+"]")} $message"
    }

    /*def _checkImpl(c: MacroContext)
                  (events: c.Expr[mutable.ListBuffer[TimerEvent]], message: c.Expr[String], log: c.Expr[Boolean],
                   now: c.Expr[Long], start: c.Expr[Long], lastEvent: c.Expr[Long]): c.Expr[Duration] = {
      import c.universe._
      val Literal(Constant(eventsV: mutable.ListBuffer[TimerEvent])) = events.tree
      val Literal(Constant(messageV: String)) = message.tree
      val Literal(Constant(logV: Boolean)) = log.tree
      val Literal(Constant(nowV: Long)) = now.tree
      val Literal(Constant(startV: Long)) = start.tree
      val Literal(Constant(lastEventV: Long)) = lastEvent.tree
      val t = nowV - startV
      val split = nowV - lastEventV
      val event = TimerEvent(nowV, t, split, messageV, None)
      eventsV += event
      if (logV) $d(event.toString)
      val duration = split.milliseconds
      c.Expr[Duration](Literal(Constant(duration)))
    }*/
  }

  def startTimer(name: String = "TIMER", silent: Boolean = false) = new ExecutionTimer(systemTime, name, silent, false)
  def disabledTimer(name: String = "TIMER") = new ExecutionTimer(systemTime, name, true, true)
}
