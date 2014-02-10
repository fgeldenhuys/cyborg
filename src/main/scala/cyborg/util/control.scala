package cyborg.util

import cyborg.util.execution.ScheduledExecutionContext
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.control.Exception._
import scala.util.{Failure, Success}
import scalaz._, Scalaz._

object control {
  implicit class OptionExt[A](val x: Option[A]) extends AnyVal {
    def failWith(f: => Any): Option[A] = {
      if (x.isDefined) x
      else { f; x }
    }
  }

  def stackTraceHandler[T](failed: T) = handling(classOf[Exception]) by { ex =>
    ex.printStackTrace()
    failed
  }

  def tryOption[T](f: => T): Option[T] = {
    try {
      Option(f)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        None
    }
  }

  def tryElse[T](f: => T)(e: Exception => T) = handling(classOf[Exception])

  def retryOnException[T](times: Int)(f: => T): Throwable \/ T =
    retryOnExceptionHelper(times, () => f)

  @tailrec private def retryOnExceptionHelper[T](times: Int, f: () => T): Throwable \/ T = {
    val result = try {
      \/-(f())
    }
    catch {
      case e: Throwable =>
        if (times <= 0) e.printStackTrace()
        -\/(e)
    }
    if (result.isLeft && times > 0)
      retryOnExceptionHelper(times - 1, f)
    else
      result
  }

  case class NotImplemented(message: String = "Not implemented") extends Exception(message)
  case class BreakException() extends Exception

  def monitor[A,P]: Monitor[A,P] = new Monitor[A,P]()

  // monitor a result [A] and see progress in units of [P]
  class Monitor[A,P](
    val promise: Promise[A] = scala.concurrent.promise[A](),
    var maxProgress: Option[P] = None,
    val progressObservers: mutable.ArrayBuffer[(P) => Any] = mutable.ArrayBuffer.empty[(P) => Any],
    val maxProgressObservers: mutable.ArrayBuffer[(P) => Any] = mutable.ArrayBuffer.empty[(P) => Any],
    var tracking: Option[Monitor[_,P]] = None
  ) {
    import Monitor._

    def reportProgress(progress: P) { progressObservers.map(_(progress)) }

    def future: Future[A] = promise.future
    def isCompleted = promise.isCompleted
    def failure(t: Throwable) = promise failure t
    def success(v: A) = promise success v

    def cancel(reason: String) {
      tracking map { other =>
        other.cancel(reason)
      } getOrElse {
        promise failure Cancelled(reason)
      }
    }

    def withMaxProgress(max: P) = {
      maxProgress = Some(max)
      maxProgressObservers.map(_(max))
      this
    }

    def onProgress(f: (P) => Any) = {
      progressObservers += f
      this
    }

    def onMaxProgress(f: (P) => Any) = {
      maxProgressObservers += f
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
      tracking = Some(other)
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
