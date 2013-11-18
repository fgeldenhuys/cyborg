package cyborg.widget

import android.util.AttributeSet
import cyborg.Context.Context
import android.content.res.TypedArray

object StyledAttributeSetReader {

  trait AttributeSetReader[A] {
    def apply(a: TypedArray, index: Int, default: A): A
  }

  implicit val intAttributeSetReader = new AttributeSetReader[Int] {
    def apply(a: TypedArray, index: Int, default: Int): Int = a.getInt(index, default)
  }

  implicit val stringAttributeSetReader = new AttributeSetReader[String] {
    def apply(a: TypedArray, index: Int, default: String): String =
      Option(a.getString(index)) getOrElse default
  }

  class StyledAttributeSetReader(styleable: Array[Int])(implicit context: Context) {
    def apply[A](attrs: AttributeSet, index: Int, default: A)
                (implicit reader: AttributeSetReader[A]): A = {
      val a = context.obtainStyledAttributes(attrs, styleable)
      try {
        reader(a, index, default)
      }
      finally {
        a.recycle()
      }
    }
  }

}
