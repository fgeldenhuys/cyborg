package cyborg.net

import cyborg.util.string.Path
import java.net.{ URI => JURI }
import scalaz._, Scalaz._

class URI(val o: Option[JURI]) {
  def scheme = o.flatMap(x => Option(x.getScheme))

  def query = o.flatMap(x => Option(x.getQuery))

  def fragment = o.flatMap(x => Option(x.getFragment))

  def host = o.flatMap(x => Option(x.getHost))

  def path = o.flatMap(x => Option(x.getPath)).map(Path(_))

  def authority = o.flatMap(x => Option(x.getAuthority))

  def port = o.flatMap(x => {
    val port = x.getPort
    if (port < 0) None else Some(port)
  })

  override def toString: String = o map (_.toString) getOrElse ""
}

object URI {
  trait CanMakeUri[A] {
    def makeUri(a: A): URI
  }

  def makeUri[A](a: A)(implicit C: CanMakeUri[A]) = C.makeUri(a)

  implicit object CanMakeUriFromString extends CanMakeUri[String] {
    override def makeUri(a: String): URI = new URI(\/.fromTryCatch(new JURI(a)).toOption)
  }

  implicit object CanMakeUriFromPath extends CanMakeUri[Path] {
    override def makeUri(a: Path): URI = URI.makeUri(a.shows)
  }

  @deprecated("Use makeUri") def apply(uri: String): URI =
    new URI(try {
      Option(new JURI(uri))
    } catch {
      case _: Throwable => None
    })
}
