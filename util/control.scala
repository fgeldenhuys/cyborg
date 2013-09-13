package cyborg.util

import scala.util.control.Exception._
import scala.Some

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

  case class NotImplemented(message: String = "Not implemented") extends Exception(message)
  case class BreakException() extends Exception
}
