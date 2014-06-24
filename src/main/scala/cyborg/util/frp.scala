package cyborg.util

import _root_.io.dylemma.frp._
import scala.concurrent.duration._
import cyborg.util.execution._

object frp {
  class EventVar[A](initial: A)(implicit obs: Observer) extends EventSource[A] {
    private var _value = initial
    def value: A = _value
    override def fire(event: A) {
      if (!stopped) _value = event
      super.fire(event)
    }
  }

  abstract class EventPipeVar[A, B](initial: A, parent: EventStream[B])
                                   (implicit obs: Observer) extends EventSource[A] {
    private var _value = initial
    def value: A = _value
    def handle(event: B): A
    parent foreach { event: B =>
      if (!stopped) _value = handle(event)
      fire(_value)
    }
  }

  implicit def value[A](ev: EventVar[A]): A = ev.value
  implicit def value[A](ev: EventPipeVar[A, _]): A = ev.value

  def eventVar[A](initial: A)(implicit obs: Observer): EventVar[A] = new EventVar[A](initial)

  def eventPipeVar[A, B](initial: A, parent: EventStream[B])(handler: B => A)
                        (implicit obs: Observer): EventPipeVar[A, B] = {
    new EventPipeVar[A, B](initial, parent) {
      override def handle(event: B): A = handler(event)
    }
  }

  def eventOption[A](parent: EventStream[A])(implicit obs: Observer): EventPipeVar[Option[A], A] = {
    new EventPipeVar[Option[A], A](None, parent) {
      override def handle(event: A): Option[A] = Some(event)
    }
  }

  class EventDebouncePipe[A](val parent: EventStream[A], val delay: Duration)
                            (implicit val sec: ScheduledExecutionContext, obs: Observer) extends EventSource[A] {
    var task: Option[ScheduledFuture[Unit]] = None
    private def handle(event: A) {
      task foreach (_.scheduled.cancel(true))
      task = Some(execute {
        fire(event)
        task = None
      } afterDelayOf delay)
    }
    parent foreach { event: A =>
      if (!stopped) handle(event)
    }
  }

  def debounced[A](parent: EventStream[A], delay: Duration)(implicit sec: ScheduledExecutionContext, obs: Observer) =
    new EventDebouncePipe[A](parent, delay)
}
