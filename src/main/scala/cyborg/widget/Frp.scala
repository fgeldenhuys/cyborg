package cyborg.widget

import android.text.{TextWatcher, Editable}
import cyborg.util.execution.ScheduledExecutionContext
import io.dylemma.frp._
import cyborg.util.frp._

object Frp {
  def clicks(v: android.view.View)
            (implicit sec: ScheduledExecutionContext): EventStream[Unit] = {
    import android.view.View, View.OnClickListener
    val es = EventSource[Unit]
    v.setOnClickListener(new OnClickListener {
      override def onClick(v: View): Unit = { es attemptFire Unit }
    })
    es
  }

  def assocClicks[A](associations: (android.view.View, A)*)
                    (implicit sec: ScheduledExecutionContext): EventStream[A] = {
    import android.view.View, View.OnClickListener
    val es = EventSource[A]
    for ((v, i) <- associations) {
      v.setOnClickListener(new OnClickListener {
        override def onClick(p1: View): Unit = { es attemptFire i }
      })
    }
    es
  }

  sealed trait TextWatcherEvent
  case class BeforeTextChangedEvent(text: CharSequence, start: Int, count: Int, after: Int) extends TextWatcherEvent
  case class TextChangedEvent(text: CharSequence, start: Int, before: Int, count: Int) extends TextWatcherEvent
  case class AfterTextChangedEvent(text: Editable) extends TextWatcherEvent

  def textChanges(tv: android.widget.TextView)
                 (implicit sec: ScheduledExecutionContext): EventStream[TextWatcherEvent] = {
    val es = EventSource[TextWatcherEvent]
    tv.addTextChangedListener(new TextWatcher {
      override def beforeTextChanged(text: CharSequence, start: Int, count: Int, after: Int): Unit = {
        es attemptFire BeforeTextChangedEvent(text, start, count, after)
      }
      override def onTextChanged(text: CharSequence, start: Int, count: Int, after: Int): Unit = {
        es attemptFire TextChangedEvent(text, start, count, after)
      }
      override def afterTextChanged(text: Editable): Unit = {
        es attemptFire AfterTextChangedEvent(text)
      }
    })
    es
  }

  def onlyTextChanges(tv: android.widget.TextView)
                     (implicit sec: ScheduledExecutionContext): EventStream[TextChangedEvent] = {
    val es = EventSource[TextChangedEvent]
    tv.addTextChangedListener(new TextWatcher {
      override def beforeTextChanged(text: CharSequence, start: Int, count: Int, after: Int): Unit = {}
      override def onTextChanged(text: CharSequence, start: Int, count: Int, after: Int): Unit = {
        es <== TextChangedEvent(text, start, count, after)
      }
      override def afterTextChanged(text: Editable): Unit = {}
    })
    es
  }
}
