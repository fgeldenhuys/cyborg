package cyborg.util

import cyborg.util.execution.ScheduledExecutionContext
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.control.Exception._
import scala.util.{Failure, Success}

object control {
  implicit class OptionExt[A](val x: Option[A]) extends AnyVal {
    def failWith(f: => Any): Option[A] = {
      if (x.isDefined) x
      else { f; x }
    }
  }

  def OptionWithFail[A](x: A)(f: => Any): Option[A] = {
    if (x == null) { f; None }
    else Some(x)
  }

  def stackTraceHandler[T](failed: T) = handling(classOf[Exception]) by { ex =>
    ex.printStackTrace()
    failed
  }

  def tryElse[T](f: => T)(e: Exception => T) = handling(classOf[Exception])

  case class NotImplemented(message: String = "Not implemented") extends Exception(message)
  case class BreakException() extends Exception

  def monitor[A,P]: Monitor[A,P] = new Monitor[A,P]()

  // monitor a result [A] and see progress in units of [P]
  class Monitor[A,P](
    val promise: Promise[A] = scala.concurrent.promise[A](),
    var maxProgress: Option[P] = None,
    val progressObservers: mutable.ArrayBuffer[(P) => Any] = mutable.ArrayBuffer.empty[(P) => Any]
  ) {
    import Monitor._

    def reportProgress(progress: P) { progressObservers.map(_(progress)) }

    def future: Future[A] = promise.future
    def isCompleted = promise.isCompleted
    def failure(t: Throwable) = promise failure t
    def success(v: A) = promise success v

    def cancel(reason: String) {
      promise failure Cancelled(reason)
    }

    def withMaxProgress(max: P) = {
      maxProgress = Some(max)
      this
    }

    def onProgress(f: (P) => Any) = {
      progressObservers += f
      this
    }

    def onSuccess(f: (A) => Any)(implicit sec: ScheduledExecutionContext) = {
      future.onSuccess {
        case result => f(result)
      }
      this
    }

    def onFailure(pf: PartialFunction[Throwable, Any])(implicit sec: ScheduledExecutionContext) = {
      future.onFailure(pf)
      this
    }

    def track[B](other: Monitor[B,P])(f: (B) => A)(implicit sec: ScheduledExecutionContext) {
      other.future onComplete {
        case Success(result) => promise success f(result)
        case Failure(t) => promise failure t
      }
      other.onProgress { p => reportProgress(p) }
      if (other.maxProgress.isDefined) maxProgress = other.maxProgress
    }
  }

  object Monitor {
    case class Cancelled(message: String) extends Exception(message)
  }


}
