package cyborg.net

import cyborg.util.string.Path
import java.net.URLEncoder
import scalaz._
import scala.collection.JavaConversions._

object Uri2 {
  trait ToUri[A] {
    def toUri(a: A): java.net.URI
  }

  implicit class ToUriOps[A](val a: A) extends AnyRef {
    def toUri(implicit U: ToUri[A]) = U.toUri(a)
    def toUriOption(implicit U: ToUri[A]) =
      \/.fromTryCatch(U.toUri(a)).leftMap(t => cyborg.Log.$w(t.getStackTraceString)).toOption
  }

  implicit class UriMethods(val uri: java.net.URI) extends AnyVal {
    def authority = Option(uri.getAuthority)
    def fragment = Option(uri.getFragment)
    def host = Option(uri.getHost)
    def path = Option(Path(uri.getPath))
    def port = Option(uri.getPort)
    def query = Option(uri.getQuery)
    def scheme = Option(uri.getScheme)
  }

  implicit object StringToUri extends ToUri[String] {
    override def toUri(a: String): java.net.URI = new java.net.URI(a)
  }

  implicit object PathToUri extends ToUri[Path] {
    override def toUri(a: Path): java.net.URI = new java.net.URI(a.toString)
  }

  implicit class URIStringContext(val sc: StringContext) extends AnyVal {
    def uri(args: Any*): java.net.URI = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buffer = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buffer append URLEncoder.encode(expressions.next.toString, "UTF-8")
        buffer append strings.next
      }
      new java.net.URI(buffer.toString)
    }

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

  trait ConvertToGetParamString[A] {
    def apply(a: A): String
  }

  implicit class ConvertToGetParamStringOps[A](val a: A) extends AnyVal {
    def asGetParamString(implicit C: ConvertToGetParamString[A]) = C.apply(a)
  }

  implicit object ConvertListPairToGetParamString
      extends ConvertToGetParamString[List[(String, String)]] {
    override def apply(a: List[(String, String)]): String = {
      import org.apache.http.client.utils.URLEncodedUtils
      import org.apache.http.message.BasicNameValuePair
      if (a.isEmpty) ""
      else "?" + URLEncodedUtils.format(a.map { x =>
        new BasicNameValuePair(x._1, x._2)
      }.toList, "utf-8")
    }
  }
}
