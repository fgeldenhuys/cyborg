package cyborg.util

import java.util.concurrent._
import java.util.concurrent.locks.Lock

import scala.concurrent.duration._

import scalaz._, Scalaz._
import scalaz.concurrent._

import cyborg.util.execution.ScheduledExecutionContext

object task {
  class UiThread private (val runOnUiThread: Runnable => Unit) {
    def apply[U](f: => U): Unit = {
      \/.fromTryCatchNonFatal {
        runOnUiThread(new Runnable {
          override def run(): Unit = {
            val result = f
          }
        })
      } .leftMap { t =>
        t.printStackTrace()
      }
    }
  }

  object UiThread {
    def apply(activity: android.app.Activity): UiThread =
      new UiThread(activity.runOnUiThread)
  }

  object CyborgTaskObjectExt {
    // Replacement for .apply without a default ExecutorService
    def create[A](a: => A)(implicit pool: ExecutorService): Task[A] = Task.apply(a)(pool)

    // Replacement for .fork without a default ExecutorService
    def forked[A](a: => Task[A])(implicit pool: ExecutorService): Task[A] = Task.fork(a)(pool)

    def uiThread[A](f: => A)(implicit UIT: UiThread): Task[A] = {
      Task.async { register =>
        \/.fromTryCatchNonFatal {
          UIT.runOnUiThread(new Runnable {
            override def run(): Unit = {
              val result = f // Force evaluation
              register(\/-(result))
            }
          })
        } .fold({ t =>
          register(-\/(t))
        }, _ => ())
      }
    }

    def delayThrowableV[A](a: Throwable \/ A): Task[A] =
      Task.delay(a).map(_.fold(throw _, identity))

    def sleep[A](millis: Long)(implicit pool: ExecutorService): (A => Task[A]) =
      (a: A) => Task[A] {
        \/.fromTryCatch(Thread.sleep(millis)).fold({
          case _: InterruptedException => a
          case other => throw other
        }, _ => a)
      } (pool)
  }
  implicit def CyborgTaskObjectExtImplicit(x: Task.type) = CyborgTaskObjectExt

  implicit class CyborgTaskExt[A](val t: Task[A]) extends AnyVal {
    def andThen[B](s: Task[B]): Task[B] = t.flatMap(_ => s)

    def runAsyncLogError(): Unit = t.runAsync(_.leftMap(e => cyborg.Log.$e(e.getStackTraceString)))

    // Discard task result or failure (which will get logged) and always return
    // Unit value. This Task is therefore run for it's side-effects.
    def discardResult: Task[Unit] = t.attempt.map(_.fold({ t =>
      cyborg.Log.$e(t.getMessage)
      t.printStackTrace()
    }, _ => ()))

    @deprecated("Use built in .after method", ":)")
    def runAfterDelay(delay: Duration)
                     (f: Throwable \/ A => Unit)
                     (implicit sec: ScheduledExecutionContext): ScheduledFuture[_] = {
      val st = debug.getStackTrace
      sec.schedule(new Runnable {
        override def run(): Unit = {
          f(t.attemptRun.leftMap { t =>
            cyborg.Log.$e("ERROR in runfterDelay created at: " + st)
            t
          })
        }
      }, delay.toMillis, TimeUnit.MILLISECONDS)
    }

    def runRepeatedlyWithDelay(delay: Duration)
                              (implicit sec: ScheduledExecutionContext): ScheduledFuture[_] = {
      val st = debug.getStackTrace
      sec.scheduleWithFixedDelay(new Runnable {
        override def run(): Unit = {
          t.attemptRun.leftMap { t =>
            cyborg.Log.$e("ERROR in runRepeatedlyWithDelay: " + t.getStackTraceString + "\nCreated at: " + st)
            t
          }
        }
      }, 0, delay.toMillis, TimeUnit.MILLISECONDS)
    }

    def runRepeatedAtInterval(interval: Duration)
                             (implicit sec: ScheduledExecutionContext): ScheduledFuture[_] = {
      val st = debug.getStackTrace
      sec.scheduleAtFixedRate(new Runnable {
        override def run(): Unit = {
          t.attemptRun.leftMap { t =>
            cyborg.Log.$e("ERROR in runRepeatedAtInterval: " + t.getStackTraceString + "\nCreated at: " + st)
          }
        }
      }, 0, interval.toMillis, TimeUnit.MILLISECONDS)
    }

    def toRunnable: Runnable = new Runnable {
      override def run(): Unit = {
        t.attemptRun.leftMap { t =>
          cyborg.Log.$e("ERROR in toRunnable: " + t)
        }
      }
    }

    def runTimed(millis: Long)(implicit sec: ScheduledExecutionContext): Unit = {
      t.timed(millis).attemptRun.fold({
        case t: java.util.concurrent.TimeoutException =>
          cyborg.Log.$e("TIMED OUT: " +  t.getStackTraceString)
        case t: Throwable =>
          cyborg.Log.$e("ERROR in runTimed: " +  t.getStackTraceString)
      }, identity)
    }

    def runTimedWithDefault(millis: Long, default: A)(implicit sec: ScheduledExecutionContext): A = {
      t.timed(millis).attemptRun.fold({
        case t: java.util.concurrent.TimeoutException =>
          cyborg.Log.$e("TIMED OUT: " +  t.getStackTraceString)
          default
        case t: Throwable =>
          cyborg.Log.$e("ERROR in runTimed: " +  t.getStackTraceString)
          default
      }, identity)
    }

    /*def withTimeWarning(millis: Long, warning: String)(implicit sec: ScheduledExecutionContext): Task[A] = {
      import scala.concurrent.duration._
      Task.create {
        val p = Promise.emptyPromise[Unit]
        Task.create {
          if (!p.fulfilled) {
            cyborg.Log.$w(warning)
          }
          else {
            cyborg.Log.$d("OP finished in time yay!")
          }
        } .after(millis.milliseconds).run
        t.onFinish(_ => Task.delay(p.fulfill(()))).run
      }
    }*/

    def exclusive(lock: Lock, millis: Long)
                 (implicit sec: ScheduledExecutionContext): Task[A] = Task.create {
      if (lock.tryLock(millis, TimeUnit.MILLISECONDS)) {
        try { t.run }
        finally { lock.unlock() }
      }
      else {
        throw new TimeoutException("Timeout waiting for lock")
      }
    }

    def exclusive(sema: Semaphore, millis: Long)
                 (implicit sec: ScheduledExecutionContext): Task[A] = Task.create {
      if (sema.tryAcquire(millis, TimeUnit.MILLISECONDS)) {
        try { t.run }
        finally { sema.release() }
      }
      else {
        throw new TimeoutException("Timeout waiting for semaphore")
      }
    }

  }

  case object NoneToTaskException extends Exception("None.toTask")

  // Convert strict Option value to Task
  implicit class CyborgOptionExtForTask[A](val o: Option[A]) extends AnyVal {
    def toTask: Task[A] =
      o.cata(a => Task.now(a), Task.fail(NoneToTaskException))
  }

  implicit class CyborgPromiseExtForTask[A](val p: Promise[A]) extends AnyVal {
    def toTask(implicit pool: ExecutorService): Task[A] = Task.fork {
      Task.async[A] { register =>
        register(\/.fromTryCatch(p.get))
      }
    } (pool)
  }
}
