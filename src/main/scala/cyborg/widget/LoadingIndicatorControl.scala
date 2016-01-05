package cyborg.widget

import scala.concurrent.duration._
import cyborg.Log._
import cyborg.util.execution.ScheduledExecutionContext
import cyborg.util.task._
import scalaz.concurrent.Task
import scalaz._, Scalaz._

/*
    This controller lets you show/hide a shared loading indicator resource.
    It hands out a token when you call show, through which you can hide it again.
    Each token has a limited life for safety, and will hide itself after a timeout.
    The loading indicator resource will keep showing so long as someone still holds an active token.
*/

class LoadingIndicatorControl[A](val showFun: A => Unit, val hideFun: () => Unit) {
  import LoadingIndicatorControl._
  private var tokens = List.empty[LoadingIndicatorToken]

  def forceHide(): Unit = {
    synchronized {
      tokens = List.empty
      hideFun()
    }
  }
}

object LoadingIndicatorControl {
  class LoadingIndicatorToken private (val controller: LoadingIndicatorControl[_], val message: String,
                                       val hideFun: () => Any, timeout: Duration)
                                      (implicit val sec: ScheduledExecutionContext) {
    private val st = cyborg.util.debug.getStackTrace

    controller.synchronized {
      controller.tokens = controller.tokens :+ this
    }

    private val hideTask = Task.delay {
      controller.synchronized {
        //$d(s"hide LOADING window $message")
        controller.tokens = controller.tokens.filterNot(_ == this)
        if (controller.tokens.isEmpty) hideFun()
      }
    }

    private val scheduledHide = hideTask.runAfterDelay(timeout) { _ =>
      $w(s"LOADING token $message timed out, created at: " + st)
    }

    def hide(): LoadingIndicatorToken = {
      scheduledHide.cancel(false)
      hideTask.attemptRun leftMap { t =>
        $w(s"Attempt to hide $message failed: " + t)
      }
      this
    }

    def hideAfter(delay: Duration): LoadingIndicatorToken = {
      scheduledHide.cancel(false)
      hideTask.runAfterDelay(delay)(_.leftMap(t => $w(s"Attempt to hide $message after delay failed: " + t)))
      this
    }
  }

  private object LoadingIndicatorToken {
    def make(controller: LoadingIndicatorControl[_], message: String, hideFun: () => Any, timeout: Duration)
            (implicit sec: ScheduledExecutionContext): LoadingIndicatorToken = {
      new LoadingIndicatorToken(controller, message, hideFun, timeout)(sec)
    }
  }

  def showLoading[A](message: A, timeout: Duration)
                    (implicit C: LoadingIndicatorControl[A],
                     sec: ScheduledExecutionContext,
                     S: Show[A]): LoadingIndicatorToken = {
    //$d(s"show LOADING window ${message.shows}")
    C.showFun(message)
    LoadingIndicatorToken.make(C, message.shows, C.hideFun, timeout)
  }
}
