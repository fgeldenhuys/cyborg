package cyborg.net

import java.net.URI

object URIExt {
  implicit class URIExt(val uri: URI) extends AnyVal {
    def filter(p: URI => Boolean): Option[URI] = {
      if (p(uri)) Some(uri)
      else None
    }
  }

  def URI(uri: String): URI = new URI(uri)

  object URIAuthority {
    def unapply(uri: URI): Option[String] = Option(uri.getAuthority)
  }
  object URIHost {
    def unapply(uri: URI): Option[String] = Option(uri.getHost)
  }
  object URIPath {
    def unapply(uri: URI): Option[String] = Option(uri.getPath)
  }
  object URIPort {
    def unapply(uri: URI): Option[Int] = uri.getPort match {
      case -1 => None
      case port => Some(port)
    }
  }
  object URIScheme {
    def unapply(uri: URI): Option[String] = Option(uri.getScheme)
  }
  object URIQuery {
    def unapply(uri: URI): Option[String] = Option(uri.getQuery)
  }
  object URIFragment {
    def unapply(uri: URI): Option[String] = Option(uri.getFragment)
  }
}
