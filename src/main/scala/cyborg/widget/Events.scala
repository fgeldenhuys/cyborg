package cyborg.widget

import android.text.{Editable, TextWatcher}
import android.view.View
import android.view.View.OnClickListener
import android.widget.AdapterView
import scalaz._, Scalaz._

object Events {
  implicit class ViewEvents(val view: android.view.View) extends AnyVal {
    def onClick(f: => Any): Unit = { view.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = { f }
    })}
  }

  implicit class AdapterViewEvents(val av: AdapterView[_]) extends AnyVal {
    def onItemSelected(f: (Option[Int]) => Any): Unit = { av.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener {
      def onItemSelected(parent: AdapterView[_], view: View, i: Int, id: Long): Unit = { f(i.some) }
      def onNothingSelected(parent: AdapterView[_]): Unit = { f(none) }
    })}
  }

  implicit class TextViewEvents(val textView: android.widget.TextView) extends AnyVal {
    def onTextChanged(f: (CharSequence) => Any): Unit = { textView.addTextChangedListener(new TextWatcher {
      val textChangedFun = (text: CharSequence) => f(text)
      def beforeTextChanged(p1: CharSequence, p2: Int, p3: Int, p4: Int): Unit = {}
      def onTextChanged(text: CharSequence, p2: Int, p3: Int, p4: Int): Unit = { textChangedFun(text) }
      def afterTextChanged(p1: Editable): Unit = {}
    })}
  }

  implicit class ListViewEvents(val list: android.widget.ListView) extends AnyVal {
    def onItemClick(f: (Int) => Any): Unit = { list.setOnItemClickListener(new AdapterView.OnItemClickListener {
      def onItemClick(parent: AdapterView[_], view: View, i: Int, id: Long): Unit = { f(i) }
    })}
  }
}