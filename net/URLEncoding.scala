package cyborg.net

import java.net.URLEncoder

object URLEncoding {
  implicit class URLEncodedStringContext(val sc: StringContext) extends AnyVal {
    def url(args: Any*): String = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buffer = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buffer append URLEncoder.encode(expressions.next.toString, "UTF-8")
        buffer append strings.next
      }
      buffer.toString
    }
  }
}
