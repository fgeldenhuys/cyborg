package cyborg.graphics

import android.graphics.{Paint => AP, Canvas, Bitmap}
import scala.collection.mutable
import scala.util.control.Exception._

class HitMask[A](width: Int, height: Int) {
  import cyborg.Log._

  val bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888)
  val canvas = new Canvas(bitmap)
  val byKey = mutable.Map.empty[Int, A]
  val byValue = mutable.Map.empty[A, AP]
  clear()

  private def emptyKey: Option[Int] = (1 until 16777216).find(byKey.get(_).isEmpty)

  def clear() { bitmap.eraseColor(Color.Black - (0xFF << 24)) }

  def key(value: A): Option[AP] = {
    byValue.get(value) orElse {
      emptyKey map { key =>
        byKey(key) = value
        val paint = Paint(color = (key | Color.OpaqueMask))
        byValue(value) = paint
        paint
      }
    }
  }

  def hit(x: Int, y: Int): Option[A] = {
    nonFatalCatch opt {
      val key = bitmap.getPixel(x, y) & Color.TransparentMask
      byKey.get(key).get
    }
  }

}