package cyborg.widget

import android.util.AttributeSet
import android.content.Context
import android.content.res.TypedArray
import android.graphics.drawable.Drawable
import cyborg.graphics.Color

object StyledAttributeSetReader {
  case class Dimension(value: Float) {
    def toInt = value.toInt
    def toFloat = value
  }

  trait AttributeSetReader[A] {
    def apply(a: TypedArray, index: Int): Option[A]
  }

  implicit val intAttributeSetReader = new AttributeSetReader[Int] {
    def apply(a: TypedArray, index: Int): Option[Int] =
      if (a.hasValue(index)) Some(a.getInt(index, 0)) else None
  }

  implicit val floatAttributeSetReader = new AttributeSetReader[Float] {
    def apply(a: TypedArray, index: Int): Option[Float] =
      if (a.hasValue(index)) Some(a.getFloat(index, 0)) else None
  }

  implicit val dimensionAttributeSetReader = new AttributeSetReader[Dimension] {
    def apply(a: TypedArray, index: Int): Option[Dimension] =
      if (a.hasValue(index)) Some(Dimension(a.getDimension(index, 0))) else None
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

  class StyledAttributeSetReader(styleable: Array[Int], context: Context) {
    def apply[A](attrs: AttributeSet, index: Int)
                (implicit reader: AttributeSetReader[A]): Option[A] = {
      val a = context.obtainStyledAttributes(attrs, styleable)
      try {
        reader(a, index)
      }
      catch {
        case t: Throwable =>
          cyborg.Log.$w(t.getMessage)
          //t.printStackTrace()
          None
      }
      finally {
        a.recycle()
      }
    }
  }

}
