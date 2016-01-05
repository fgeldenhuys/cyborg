package cyborg.util

import scalaz._, Scalaz._

object duration {
  trait HasDuration[A] {
    def duration(a: A): Duration
    def microseconds(a: A): Long = duration(a).microseconds
    def milliseconds(a: A): Long = microseconds(a) / 1000
    def seconds(a: A): Long = milliseconds(a) / 1000
    def minutes(a: A): Long = seconds(a) / 60
    def hours(a: A): Long = minutes(a) / 60
  }

  def duration[A](a: A)(implicit D: HasDuration[A]) = D.duration(a)
  def microseconds[A](a: A)(implicit D: HasDuration[A]) = D.microseconds(a)
  def milliseconds[A](a: A)(implicit D: HasDuration[A]) = D.milliseconds(a)
  def seconds[A](a: A)(implicit D: HasDuration[A]) = D.seconds(a)
  def minutes[A](a: A)(implicit D: HasDuration[A]) = D.minutes(a)
  def hours[A](a: A)(implicit D: HasDuration[A]) = D.hours(a)

  case class Duration(microseconds: Long) {
    def toScalaStd = scala.concurrent.duration.Duration(microseconds, scala.concurrent.duration.MICROSECONDS)
  }

  object Duration {
    def Max = Duration(Long.MaxValue)
  }

  object Microseconds {
    def apply(x: Long) = Duration(x)
    def unapply(d: Duration): Option[Long] = Some(d.microseconds)
  }

  object Milliseconds {
    def apply(x: Long) = Duration(x * 1000)
    def unapply(d: Duration): Option[Long] = Some(d.microseconds / 1000)
  }

  object Seconds {
    def apply(x: Long) = Duration(x * 1000 * 1000)
    def unapply(d: Duration): Option[Long] = Some(d.microseconds / (1000 * 1000))
  }

  object Minutes {
    def apply(x: Long) = Duration(x * 1000 * 1000 * 60)
    def unapply(d: Duration): Option[Long] = Some(d.microseconds / (1000 * 1000 * 60))
  }

  object Hours {
    def apply(x: Long) = Duration(x * 1000 * 1000 * 60 * 60)
    def unapply(d: Duration): Option[Long] = Some(d.microseconds / (1000 * 1000 * 60 * 60))
  }

  def now = Milliseconds(System.currentTimeMillis())

  implicit object DurationIdentity extends HasDuration[Duration] {
    override def duration(a: Duration): Duration = a
  }

  implicit object DurationShow extends Show[Duration] {
    override def shows(f: Duration): String = {
      def divisible(x: Long, y: Int) = x / y * y == x
      if (divisible(f.microseconds, 1000 * 1000 * 60 * 60)) s"Duration(${f.microseconds / (1000 * 1000 * 60 * 60)} hours)"
      else if (divisible(f.microseconds, 1000 * 1000 * 60)) s"Duration(${f.microseconds / (1000 * 1000 * 60)} minutes)"
      else if (divisible(f.microseconds, 1000 * 1000)) s"Duration(${f.microseconds / (1000 * 1000)} seconds)"
      else if (divisible(f.microseconds, 1000)) s"Duration(${f.microseconds / 1000} milliseconds)"
      else s"Duration(${f.microseconds} microseconds)"
    }
  }

  implicit object DurationMonoid extends Monoid[Duration] {
    override def zero = Duration(0)
    override def append(f1: Duration, f2: => Duration) = Duration(f1.microseconds + f2.microseconds)
  }

  implicit object DurationOrder extends Order[Duration] {
    override def order(x: Duration, y: Duration) = {
      if (x.microseconds == y.microseconds) Ordering.EQ
      else if (x.microseconds < y.microseconds) Ordering.LT
      else Ordering.GT
    }
  }

  object DurationDSL {
    implicit class LongDurationDSL(val x: Long) extends AnyVal {
      def ms = Milliseconds(x)
      def millisecond = Milliseconds(x)
      def milliseconds = Milliseconds(x)
      def sec = Seconds(x)
      def secs = Seconds(x)
      def second = Seconds(x)
      def seconds = Seconds(x)
      def mins = Minutes(x)
      def minute = Minutes(x)
      def minutes = Minutes(x)
      def hour = Hours(x)
      def hours = Hours(x)
    }
  }
}
