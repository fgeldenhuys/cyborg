package cyborg.widget

import android.text.{TextWatcher, Editable}
import io.dylemma.frp._

object Frp {
  def clicks(v: android.view.View): EventStream[Unit] = {
    import android.view.View, View.OnClickListener
    val es = EventSource[Unit]
    v.setOnClickListener(new OnClickListener {
      override def onClick(v: View) { es fire Unit }
    })
    es
  }

  sealed trait TextWatcherEvent
  case class BeforeTextChangedEvent(text: CharSequence, start: Int, count: Int, after: Int) extends TextWatcherEvent
  case class TextChangedEvent(text: CharSequence, start: Int, before: Int, count: Int) extends TextWatcherEvent
  case class AfterTextChangedEvent(text: Editable) extends TextWatcherEvent

  def textChanges(tv: android.widget.TextView): EventStream[TextWatcherEvent] = {
    val es = EventSource[TextWatcherEvent]
    tv.addTextChangedListener(new TextWatcher {
      override def beforeTextChanged(text: CharSequence, start: Int, count: Int, after: Int) {
        es fire BeforeTextChangedEvent(text, start, count, after)
      }
      override def onTextChanged(text: CharSequence, start: Int, count: Int, after: Int) {
        es fire TextChangedEvent(text, start, count, after)
      }
      override def afterTextChanged(text: Editable) {
        es fire AfterTextChangedEvent(text)
      }
    })
    es
  }
}
