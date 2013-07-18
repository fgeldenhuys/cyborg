package cyborg.widget

import cyborg.util.NotImplemented
import android.view.View

object Accessors {
  implicit class TextViewAccessors(val tv: android.widget.TextView) extends AnyVal {
    def backgroundColour: Int = { throw NotImplemented() }
    def backgroundColour_=(c: Int) { tv.setBackgroundColor(c)}
    def colour: Int = tv.getCurrentTextColor
    def colour_=(colour: Int) { tv.setTextColor(colour) }
    def enabled: Boolean = tv.isEnabled
    def enabled_=(e: Boolean) { tv.setEnabled(e) }
    def hint = tv.getHint
    def hint_=(h: CharSequence) { tv.setHint(h) }
    def text: CharSequence = tv.getText
    def text_=(text: CharSequence) { tv.setText(text) }
    def textSize = tv.getTextSize
    def textSize_=(s: Double) { tv.setTextSize(s.toFloat) }
    def visible = tv.getVisibility == View.VISIBLE
    def visible_=(v: Boolean) { tv.setVisibility(if(v) View.VISIBLE else View.INVISIBLE) }
  }

  implicit class ProgressBarAccessors(val pb: android.widget.ProgressBar) extends AnyVal {
    def max: Int = pb.getMax
    def max_=(m: Int) { pb.setMax(m) }
    def progress: Int = pb.getProgress
    def progress_=(p: Int) { pb.setProgress(p) }
  }
}
