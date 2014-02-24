package cyborg.net

import java.net.{ URI => JURI }

class URI(o: Option[JURI]) {
  def scheme = o.flatMap(x => Option(x.getScheme))

  def query = o.flatMap(x => Option(x.getQuery))

  def fragment = o.flatMap(x => Option(x.getFragment))

  def host = o.flatMap(x => Option(x.getHost))

  def path = o.flatMap(x => Option(x.getPath))

  def authority = o.flatMap(x => Option(x.getAuthority))

  def port = o.flatMap(x => {
    val port = x.getPort
    if (port < 0) None else Some(port)
  })
}

object URI {
  def apply(uri: String): URI =
    new URI(try {
      Option(new JURI(uri))
    } catch {
      case _: Throwable => None
    })
}
