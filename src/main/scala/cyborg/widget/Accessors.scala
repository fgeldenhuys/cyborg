package cyborg.widget

import android.graphics.drawable.Drawable
import cyborg.resources.ViewResource
import cyborg.util.NotImplemented
import android.view.View
import android.widget.{SpinnerAdapter, ListAdapter}
import scalaz._

object Accessors {
  implicit class ViewAccessors(val v: android.view.View) extends AnyVal {
    def clickable = v.isClickable
    def clickable_=(b: Boolean): Unit = { v.setClickable(b) }
    def find[A](r: ViewResource[A]) = v.findViewById(r.id).asInstanceOf[A]
    def height = v.getHeight
    def visible = v.getVisibility == View.VISIBLE
    def visible_=(b: Boolean): Unit = \/.fromTryCatch {
      v.setVisibility(if (b) View.VISIBLE else View.INVISIBLE)
    }.leftMap(_.printStackTrace())
    def width = v.getWidth
  }

  implicit class ButtonAccessors(val b: android.widget.Button) extends AnyVal {
    def focus(): Unit = { b.setFocusableInTouchMode(true); b.requestFocus() }
  }

  implicit class GridViewAccessors(val gv: android.widget.GridView) extends AnyVal {
    def numColumns = gv.getNumColumns
    def numColumns_=(c: Int): Unit = { gv.setNumColumns(c) }
  }

  implicit class ImageViewAccessors(val v: android.widget.ImageView) extends AnyVal {
    def drawable = v.getDrawable
    def drawable_=(d: Drawable): Unit = { v.setImageDrawable(d) }
  }

  implicit class ListViewAccessors(val v: android.widget.ListView) extends AnyVal {
    def adapter: ListAdapter = v.getAdapter
    def adapter_=(a: ListAdapter): Unit = { v.setAdapter(a) }
    def selected: Int = v.getSelectedItemPosition
    def selected_=(i: Int): Unit = { v.setSelection(i) }
  }

  implicit class ProgressBarAccessors(val pb: android.widget.ProgressBar) extends AnyVal {
    def max: Int = pb.getMax
    def max_=(m: Int): Unit = { pb.setMax(m) }
    def progress: Int = pb.getProgress
    def progress_=(p: Int): Unit = { pb.setProgress(p) }
  }

  implicit class SpinnerAccessors(val v: android.widget.Spinner) extends AnyVal {
    def adapter: SpinnerAdapter = v.getAdapter
    def adapter_=(a: SpinnerAdapter): Unit = { v.setAdapter(a) }
    def selected: Int = v.getSelectedItemPosition
    def selected_=(i: Int): Unit = { v.setSelection(i) }
  }

  implicit class TextViewAccessors(val tv: android.widget.TextView) extends AnyVal {
    def backgroundColour: Int = { throw NotImplemented() }
    def backgroundColour_=(c: Int): Unit = { tv.setBackgroundColor(c)}
    def colour: Int = tv.getCurrentTextColor
    def colour_=(colour: Int): Unit = { tv.setTextColor(colour) }
    def enabled: Boolean = tv.isEnabled
    def enabled_=(e: Boolean): Unit = { tv.setEnabled(e) }
    def hint = tv.getHint
    def hint_=(h: CharSequence): Unit = { tv.setHint(h) }
    def text: CharSequence = tv.getText
    def text_=(text: CharSequence): Unit = { tv.setText(text) }
    def textSize = tv.getTextSize
    def textSize_=(s: Double): Unit = { tv.setTextSize(s.toFloat) }
  }

}