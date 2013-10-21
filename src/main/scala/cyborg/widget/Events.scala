package cyborg.widget

import android.text.{Editable, TextWatcher}
import android.view.View
import android.view.View.OnClickListener
import android.widget.AdapterView
import android.widget.AdapterView.OnItemClickListener

object Events {
  implicit class ButtonEvents(val button: android.widget.Button) {
    def onClick(f: => Any) { button.setOnClickListener(new OnClickListener {
      def onClick(v: View) { f }
    })}
  }
  implicit class TextViewEvents(val textView: android.widget.TextView) {
    def onTextChanged(f: (CharSequence) => Any) { textView.addTextChangedListener(new TextWatcher {
      val textChangedFun = (text: CharSequence) => f(text)
      def beforeTextChanged(p1: CharSequence, p2: Int, p3: Int, p4: Int) {}
      def onTextChanged(text: CharSequence, p2: Int, p3: Int, p4: Int) { textChangedFun(text) }
      def afterTextChanged(p1: Editable) {}
    })}
  }
  implicit class ListViewEvents(val list: android.widget.ListView) {
    def onItemClick(f: (Int) => Any) { list.setOnItemClickListener(new OnItemClickListener {
      def onItemClick(parent: AdapterView[_], view: View, i: Int, id: Long) { f(i) }
    })}
  }
}
