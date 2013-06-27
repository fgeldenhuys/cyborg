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
    def onTextChanged(f: (CharSequence) => Unit) { textView.addTextChangedListener(new TextWatcher {
      val textChangedFun = (text: CharSequence) => f(text)
      def beforeTextChanged(p1: CharSequence, p2: Int, p3: Int, p4: Int) {}
      def onTextChanged(text: CharSequence, p2: Int, p3: Int, p4: Int) { textChangedFun(text) }
      def afterTextChanged(p1: Editable) {}
    })}
  }
}
