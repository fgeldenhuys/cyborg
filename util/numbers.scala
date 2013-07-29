package cyborg.util

object numbers {
  object ValidInt {
    def unapply(i: BigInt): Option[Int] = if (i.isValidInt) Some(i.toInt) else None
  }
  object ValidLong {
    def unapply(l: BigInt): Option[Long] = if (l.isValidLong) Some(l.toLong) else None
  }

  object ImplicitDoubleFloatConversions {
    implicit def double2float(d: Double) = d.toFloat
    implicit def float2double(f: Float) = f.toDouble
  }

  object ImplicitDoubleIntConversions {
    implicit def double2int(d: Double) = d.toInt
    implicit def int2double(i: Int) = i.toDouble
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
      java.lang.Long.parseLong(WhitespaceRegex replaceAllIn (buffer.toString, _ => ""), 16).toInt
    }
  }
}
