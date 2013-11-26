package cyborg.util

import scala.util.control.Exception._
import scala.Some

object numbers {
  private val numberFormatExceptionHandler = catching(classOf[NumberFormatException])

  object ValidInt {
    def unapply(i: BigInt): Option[Int] = if (i.isValidInt) Some(i.toInt) else None
    def unapply(s: String): Option[Int] = numberFormatExceptionHandler.opt(s.toInt)
  }
  object ValidLong {
    def unapply(l: BigInt): Option[Long] = if (l.isValidLong) Some(l.toLong) else None
    def unapply(s: String): Option[Long] = numberFormatExceptionHandler.opt(s.toLong)
  }

  object ImplicitDoubleFloatConversions {
    implicit def double2float(d: Double) = d.toFloat
    implicit def float2double(f: Float) = f.toDouble
  }

  object ImplicitDoubleIntConversions {
    implicit def double2int(d: Double) = d.toInt
    implicit def int2double(i: Int) = i.toDouble
  }

  object StringNumberConversionsCyborgExt {
    import scala.util.control.Exception._

    implicit class StringNumberConversionsCyborgExt(val string: String) extends AnyVal {
      def toIntElse(d: Int) = numberFormatExceptionHandler.opt(string.toInt).getOrElse(d)
      def toShortElse(s: Short) = numberFormatExceptionHandler.opt(string.toShort).getOrElse(s)
      def toLongElse(l: Long) = numberFormatExceptionHandler.opt(string.toLong).getOrElse(l)
      def isInt = numberFormatExceptionHandler opt {string.toInt; true} getOrElse false
      def toDoubleOption = numberFormatExceptionHandler opt { string.toDouble }
    }
  }

  object BooleanNumberConversionsCyborgExt {
    implicit class BooleanNumberConversionsCyborgExt(val b: Boolean) extends AnyVal {
      def toInt = if (b) 1 else 0
    }
    implicit class IntBooleanConversionsCyborgExt(val i: Int) extends AnyVal {
      def toBoolean = i != 0
    }
  }

  private val WhitespaceRegex = "\\s".r
  implicit class HexStringContext(val sc: StringContext) extends AnyVal {
    def hex(args: Any*): Int = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buffer = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buffer append expressions.next
        buffer append strings.next
      }
      java.lang.Long.parseLong(WhitespaceRegex replaceAllIn (buffer.toString, ""), 16).toInt
    }
  }
}
