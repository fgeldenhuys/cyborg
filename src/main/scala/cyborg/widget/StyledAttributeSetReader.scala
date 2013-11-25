package cyborg.widget

import android.util.AttributeSet
import cyborg.Context.Context
import android.content.res.TypedArray
import android.graphics.drawable.Drawable
import cyborg.graphics.Color._

object StyledAttributeSetReader {

  trait AttributeSetReader[A] {
    def apply(a: TypedArray, index: Int): Option[A]
  }

  implicit val intAttributeSetReader = new AttributeSetReader[Int] {
    def apply(a: TypedArray, index: Int): Option[Int] =
      if (a.hasValue(index)) Some(a.getInt(index, 0)) else None
  }

  implicit val stringAttributeSetReader = new AttributeSetReader[String] {
    def apply(a: TypedArray, index: Int): Option[String] = Option(a.getString(index))
  }

  implicit val colorAttributeSetReader = new AttributeSetReader[Color] {
    def apply(a: TypedArray, index: Int): Option[Color] =
      if (a.hasValue(index)) Option(Color(a.getColor(index, 0))) else None
  }

  implicit val drawableAttributeSetReader = new AttributeSetReader[Drawable] {
    def apply(a: TypedArray, index: Int): Option[Drawable] = Option(a.getDrawable(index))
  }

  class StyledAttributeSetReader(styleable: Array[Int])(implicit context: Context) {
    def apply[A](attrs: AttributeSet, index: Int)
                (implicit reader: AttributeSetReader[A]): Option[A] = {
      val a = context.obtainStyledAttributes(attrs, styleable)
      try {
        reader(a, index)
      }
      finally {
        a.recycle()
      }
    }
  }

}
