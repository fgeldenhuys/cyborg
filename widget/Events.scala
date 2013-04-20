package cyborg.widget

import android.view.View.OnClickListener
import android.view.View

object Events {
  implicit class ButtonEvents(val button: android.widget.Button) {
    def onClick(f: => Unit) { button.setOnClickListener(new OnClickListener {
      def onClick(v: View) { f }
    })}
  }
}
