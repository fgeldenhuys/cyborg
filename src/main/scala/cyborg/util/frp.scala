package cyborg.util

import _root_.io.dylemma.frp._

object frp {
  abstract class EventVar[A, B](initial: A, parent: EventStream[B])
                               (implicit obs: Observer) extends EventSource[A] {
    private var _value = initial
    def value: A = _value
    def handle(event: B): A
    parent foreach { event: B =>
      _value = handle(event)
      fire(_value)
    }
  }

  implicit def eventVar2Value[A](ev: EventVar[A, _]): A = ev.value

  def eventVar[A, B](initial: A, parent: EventStream[B])(handler: B => A)(implicit obs: Observer): EventVar[A, B] = {
    new EventVar[A, B](initial, parent) {
      override def handle(event: B): A = handler(event)
    }
  }

  def eventOption[A](parent: EventStream[A])(implicit obs: Observer): EventVar[Option[A], A] = {
    new EventVar[Option[A], A](None, parent) {
      override def handle(event: A): Option[A] = Some(event)
    }
  }
}
