package cyborg.graphics

import android.graphics.{Path => AP}

object Path {
  def path = new AP()
  implicit class PathExt(val p: AP) extends AnyVal {
    def move(x: Int, y: Int): AP = {
      p.moveTo(x.toFloat, y.toFloat)
      p
    }
    def line(x: Int, y: Int): AP = {
      p.lineTo(x.toFloat, y.toFloat)
      p
    }
  }
}
