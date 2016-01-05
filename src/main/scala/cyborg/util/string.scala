package cyborg.util

import java.util.zip
import scalaz._, Scalaz._

object string {
  private val PathTrimChars = "/ \n\t"

  trait UrlEncodingOps[A] {
    def urlEncoded(a: A): A
    def urlDecoded(a: A): A
  }

  implicit class ToUrlEncodingOps[A](val a: A) extends AnyRef {
    def urlEncoded(implicit U: UrlEncodingOps[A]) = U.urlEncoded(a)
    def urlDecoded(implicit U: UrlEncodingOps[A]) = U.urlDecoded(a)
  }

  implicit object StringUrlEncodingOps extends UrlEncodingOps[String] {
    override def urlEncoded(a: String): String = java.net.URLEncoder.encode(a, "UTF-8")
    override def urlDecoded(a: String): String = java.net.URLDecoder.decode(a, "UTF-8")
  }

  implicit class CyborgStringExt(val s: String) extends AnyVal {
    def adler32: Long = {
      val checksum = new zip.Adler32
      val bytes = s.getBytes("UTF-8")
      checksum.update(bytes)
      checksum.getValue
    }

    def takeRightWhile(p: Char => Boolean): String =
      s.reverse.takeWhile(p).reverse

    def escapeChars(chars: List[Char], using: String): String = {
      val sb = new scala.collection.mutable.StringBuilder(s.size)
      s.toList.foreach { c =>
        if (chars.contains(c)) {
          sb ++= using
          sb += c
        }
        else sb += c
      }
      sb.toString
    }

    def escapeJson = escapeChars("\"\\".toList, "\\")

    def ellipses(n: Int): String = if (s.length > n) s.take(n) + "..." else s

    def toIntOpt: Option[Int] =
      \/.fromTryCatchNonFatal(s.toInt).toOption

    def isInt: Boolean = toIntOpt.isDefined

    def toFloatOpt: Option[Float] =
      \/.fromTryCatchNonFatal(s.toFloat).toOption

    def isFloat: Boolean = toFloatOpt.isDefined
  }

  final class Path private (val value: String) extends AnyVal {
    override def toString = value

    def trim = Path(value.reverse.dropWhile(PathTrimChars contains _).reverse.dropWhile(PathTrimChars contains _))
    def trimLeft = Path(value.dropWhile(PathTrimChars contains _))
    def trimRight = Path(value.reverse.dropWhile(PathTrimChars contains _).reverse)

    def startsWith(prefix: Path): Boolean =
      this.trimLeft.value.startsWith(prefix.trim.value)

    def isEmpty: Boolean = value.isEmpty

    // Strip the given path prefix from this path
    def stripPrefix(prefix: Path): Path = {
      \/.fromTryCatchNonFatal {
        val startsWithSlash = value.trim.startsWith("/")
        val stripped = this.trimLeft.value.stripPrefix(prefix.trimLeft.value)
        if (startsWithSlash && !stripped.startsWith("/")) Path("/" + stripped) else Path(stripped)
      } .getOrElse(this)
    }

    // Remove everything from first hash (#) to the end
    def stripAnchor: Path = {
      \/.fromTryCatchNonFatal {
        Path(value.substring(0, value.indexOf('#')))
      } .getOrElse(this)
    }

    // Example: "a/b/c" -> "a/b"
    def dropRightSegment: Path = {
      val endsWithSlash = value.trim.endsWith("/")
      val trimmed = this.trimRight
      val lastSlashIndex = trimmed.value.lastIndexOf('/')
      val result = if (lastSlashIndex >= 0 && lastSlashIndex <= trimmed.value.size)
        trimmed.value.substring(0, lastSlashIndex)
      else ""
      if (endsWithSlash) Path(result + "/") else Path(result)
    }

    def insertAfter(insert: Path, after: Path): Path = {
      import Path.PathMonoid.append
      \/.fromTryCatchNonFatal {
        val i = value.indexOf(after.value)
        if (i >= 0) {
          val (head, tail) = value.splitAt(i + after.value.size)
          append(append(Path(head), insert), Path(tail))
        }
        else this
      } .getOrElse (this)
    }

    def resolve: Path = {
      Path(value.split("/").toList.foldLeft(List.empty[String]) { (a, b) =>
        if (b == "." || b.isEmpty) a
        else if (b == "..") a.dropRight(1)
        else a :+ b
      } .mkString("/"))
    }

    def lastSegment: Path = {
      val trimmed = this.trimRight
      val lastSlashIndex = trimmed.value.lastIndexOf('/')
      if (lastSlashIndex >= 0)
        Path(trimmed.value.substring(lastSlashIndex + 1))
      else
        trimmed
    }
  }

  object Path {
    //TODO: make multiple //// into a single one
    def apply(s: String): Path = new Path(s)
    def unapply(p: Path): Option[String] = Option(p.value)

    implicit object PathShow extends Show[Path] {
      override def shows(f: Path): String = f.value
    }

    implicit object PathMonoid extends Monoid[Path] {
      def zero: Path = new Path("")

      def append(f1: Path, f2: => Path): Path =
        new Path(
          if (f1.value.endsWith("/") && f2.value.startsWith("/")) f1.value + f2.value.drop(1)
          else if (f1.value.endsWith("/")) f1.value + f2.value
          else if (f2.value.startsWith("/")) f1.value + f2.value
          else f1.value + "/" + f2.value
        )
    }

    implicit object PathEqual extends Equal[Path] {
      override def equal(a: Path, b: Path): Boolean = a.value === b.value
    }
  }

  trait ToPath[A] {
    def toPath(a: A): Path
  }

  implicit class ToPathOps[A](val a: A) extends AnyRef {
    def toPath(implicit P: ToPath[A]) = P.toPath(a)
  }

  implicit object StringToPath extends ToPath[String] { override def toPath(a: String): Path = Path(a) }
  implicit object FileToPath extends ToPath[java.io.File] { override def toPath(a: java.io.File) = Path(a.getAbsolutePath) }

  implicit object PathUrlEncodingOps extends UrlEncodingOps[Path] {
    override def urlEncoded(a: Path): Path = Path(java.net.URLEncoder.encode(a.value, "UTF-8"))
    override def urlDecoded(a: Path): Path = Path(java.net.URLDecoder.decode(a.value, "UTF-8"))
  }

  implicit class PathStringContext(val sc: StringContext) extends AnyVal {
    def path(args: Any*): Path = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buffer = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buffer append expressions.next.toString
        buffer append strings.next
      }
      Path(buffer.toString)
    }
  }

}
