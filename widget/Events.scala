package cyborg.widget

import android.view.View.OnClickListener
import android.view.View
import android.text.{Editable, TextWatcher}

object Events {
  implicit class ButtonEvents(val button: android.widget.Button) {
    def onClick(f: => Unit) { button.setOnClickListener(new OnClickListener {
      def onClick(v: View) { f }
    })}
  }
  implicit class TextViewEvents(val textView: android.widget.TextView) {
    def onTextChanged(f: => Unit) { textView.addTextChangedListener(new TextWatcher {
      val textChangedFun = () => f
      def beforeTextChanged(p1: CharSequence, p2: Int, p3: Int, p4: Int) {}
      def onTextChanged(p1: CharSequence, p2: Int, p3: Int, p4: Int) { textChangedFun() }
      def afterTextChanged(p1: Editable) {}
    })}
  }
}
