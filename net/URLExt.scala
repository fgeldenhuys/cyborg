package cyborg.net

import java.net.URL

object URLExt {
  implicit class URLExt(val url: URL) extends AnyVal {
    def filter(p: URL => Boolean): Option[URL] = {
      if (p(url)) Some(url)
      else None
    }
  }

  def URL(url: String): URL = new URL(url)

  object URLAuthority {
    def unapply(url: URL): Option[String] = Option(url.getAuthority)
  }
  object URLFile {
    def unapply(url: URL): Option[String] = Option(url.getFile)
  }
  object URLHost {
    def unapply(url: URL): Option[String] = Option(url.getHost)
  }
  object URLPath {
    def unapply(url: URL): Option[String] = Option(url.getPath)
  }
  object URLPort {
    def unapply(url: URL): Option[Int] = url.getPort match {
      case -1 => None
      case port => Some(port)
    }
  }
  object URLProtocol {
    def unapply(url: URL): Option[String] = Option(url.getProtocol)
  }
  object URLQuery {
    def unapply(url: URL): Option[String] = Option(url.getQuery)
  }
  object URLRef {
    def unapply(url: URL): Option[String] = Option(url.getRef)
  }
}
