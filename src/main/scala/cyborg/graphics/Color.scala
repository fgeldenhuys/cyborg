package cyborg.graphics

import cyborg.Log._
import cyborg.util.numbers._

object Color {
  case class InvalidColorStringException(string: String) extends Exception(s"Invalid color string '$string'")

  implicit class ColorStringContext(val sc: StringContext) extends AnyVal {
    def c(args: Any*): Int = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buffer = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buffer append expressions.next
        buffer append strings.next
      }
      val str = buffer.toString
      //$d(s"make color from $str")

      if (str(0) == '#' && str.length == 3) { // #GG
        val i = str.substring(1,3)
        hex"FF$i$i$i"
      }
      else if (str(0) == '#' && str.length == 7) // #RRGGBB
        hex"FF${str.substring(1,3)}${str.substring(3,5)}${str.substring(5,7)}"
      else if (str(0) == '#' && str.length == 9) // #AARRGGBB
        hex"${str.substring(1,3)}${str.substring(3,5)}${str.substring(5,7)}${str.substring(7,9)}"
      else
        throw InvalidColorStringException(str)
    }
  }

  val OpaqueMask = 0xFF000000
  val TransparentMask = 0x00FFFFFF
  val Black = android.graphics.Color.BLACK
  val White = android.graphics.Color.WHITE
}
