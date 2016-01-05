package cyborg.graphics

import android.content.Context
import cyborg.util.numbers._

case class Color(value: Int)

object Color {
  implicit def color2int(c: Color): Int = c.value

  case class InvalidColorStringException(string: String) extends Exception(s"Invalid color string '$string'")

  implicit class ColorStringContext(val sc: StringContext) extends AnyVal {
    def c(args: Any*): Color = {
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
        Color(hex"FF$i$i$i")
      }
      else if (str(0) == '#' && str.length == 7) // #RRGGBB
        Color(hex"FF${str.substring(1,3)}${str.substring(3,5)}${str.substring(5,7)}")
      else if (str(0) == '#' && str.length == 9) // #AARRGGBB
        Color(hex"${str.substring(1,3)}${str.substring(3,5)}${str.substring(5,7)}${str.substring(7,9)}")
      else
        throw InvalidColorStringException(str)
    }
  }

  val OpaqueMask = 0xFF000000
  val TransparentMask = 0x00FFFFFF
  val Black = Color(android.graphics.Color.BLACK)
  val White = Color(android.graphics.Color.WHITE)
  val Red = Color(android.graphics.Color.RED)
  val Blue = Color(android.graphics.Color.BLUE)

  def fromResource(r: Int)(implicit context: Context) = Color(context.getResources.getColor(r))
}
