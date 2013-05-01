package cyborg.widget

import cyborg.util.NotImplemented

object Accessors {
  implicit class TextViewAccessors(val tv: android.widget.TextView) {
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
  }
}