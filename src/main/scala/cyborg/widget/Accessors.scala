package cyborg.widget

import cyborg.resources.ViewResource
import cyborg.util.NotImplemented
import android.view.View
import android.widget.{SpinnerAdapter, ListAdapter}

object Accessors {
  implicit class ViewAccessors(val v: android.view.View) extends AnyVal {
    def clickable = v.isClickable
    def clickable_=(b: Boolean) { v.setClickable(b) }
    def find[A](r: ViewResource[A]) = v.findViewById(r.id).asInstanceOf[A]
    def height = v.getHeight
    def visible = v.getVisibility == View.VISIBLE
    def visible_=(b: Boolean) { v.setVisibility(if (b) View.VISIBLE else View.INVISIBLE) }
    def width = v.getWidth
  }

  implicit class ButtonAccessors(val b: android.widget.Button) extends AnyVal {
    def focus() { b.setFocusableInTouchMode(true); b.requestFocus() }
  }

  implicit class GridViewAccessors(val gv: android.widget.GridView) extends AnyVal {
    def numColumns = gv.getNumColumns
    def numColumns_=(c: Int) { gv.setNumColumns(c) }
  }

  implicit class ListViewAccessors(val v: android.widget.ListView) extends AnyVal {
    def adapter: ListAdapter = v.getAdapter
    def adapter_=(a: ListAdapter) { v.setAdapter(a) }
    def selected: Int = v.getSelectedItemPosition
    def selected_=(i: Int) { v.setSelection(i) }
  }

  implicit class ProgressBarAccessors(val pb: android.widget.ProgressBar) extends AnyVal {
    def max: Int = pb.getMax
    def max_=(m: Int) { pb.setMax(m) }
    def progress: Int = pb.getProgress
    def progress_=(p: Int) { pb.setProgress(p) }
  }

  implicit class SpinnerAccessors(val v: android.widget.Spinner) extends AnyVal {
    def adapter: SpinnerAdapter = v.getAdapter
    def adapter_=(a: SpinnerAdapter) { v.setAdapter(a) }
    def selected: Int = v.getSelectedItemPosition
    def selected_=(i: Int) { v.setSelection(i) }
  }

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
  }

}
