package cyborg.util

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
}
