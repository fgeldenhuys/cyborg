package cyborg.net

import cyborg.util.io._
import java.io.{OutputStream, InputStream}
import org.apache.http._
import org.apache.http.client._
import org.apache.http.conn.scheme._
import org.apache.http.conn.ssl._
import org.apache.http.params._
import scalaz._, Scalaz._
import scalaz.concurrent.Task

object Http2 {

  type QueryItem = (String, Option[String])
  type Query = List[QueryItem]

  type PostQueryItem = (String, String)
  type PostQuery = List[PostQueryItem]

  def renderQuery(qmark: Boolean, query: Query): String = {
    import scalaz.syntax.std.option._
    def go(sep: String, q: QueryItem): String =
      sep + java.net.URLEncoder.encode(q._1) + q._2.cata(z => "=" + java.net.URLEncoder.encode(z), "")

    query match {
      case Nil     => ""
      case p :: ps => go(if (qmark) "?" else "", p) + ps.map(go("&", _)).mkString
    }
  }

  implicit val httpMonad: Monad[Http2] = new Monad[Http2] {
    def point[A](a: => A) =
      Http2.point(a)

    def bind[A,B](fa: Http2[A])(f: A => Http2[B]): Http2[B] =
      fa flatMap f
  }

  implicit object HttpResponseInputStreamMaker extends InputStreamMaker[HttpResponse] {
    override def makeInputStream(a: HttpResponse): Throwable \/ InputStream =
      \/.fromTryCatch(a.getEntity.getContent)
  }

  implicit object HttpResponseWriteToOutputStream extends WriteToOutputStream[HttpResponse] {
    override def write(a: HttpResponse, out: OutputStream, progress: (Int) => Unit): Task[Int] =
      Task.delay(a.getEntity.getContent).flatMap(in => InputStreamToOutputStream.write(in, out, progress))
  }


  private def createSchemes(keyStore: java.security.KeyStore) = {
    import java.net.{ InetAddress, InetSocketAddress, Socket }
    import javax.net.ssl.SSLSocket

    val schemeRegistry = new SchemeRegistry
    schemeRegistry.register(new Scheme("http", PlainSocketFactory.getSocketFactory(), 80))
    schemeRegistry.register(new Scheme("https", new SSLSocketFactory(keyStore) {
      private val socketFactory = cyborg.net.HttpSecure.makeSslSocketFactoryFromKeyStore(keyStore)

      override def connectSocket(sock: Socket, host: String, port: Int, localAddress: InetAddress, localPort: Int, params: HttpParams): Socket = {
        val sslSock = Option(sock).getOrElse(createSocket)
        sslSock.bind(new InetSocketAddress(localAddress, localPort))

        val connTimeout = HttpConnectionParams.getConnectionTimeout(params)
        val soTimeout = HttpConnectionParams.getSoTimeout(params)
        val remoteAddress = new InetSocketAddress(host, port)

        sslSock.connect(remoteAddress, connTimeout)
        sslSock.setSoTimeout(soTimeout)
        sslSock
      }

      override def createSocket(socket: Socket, host: String, port: Int, autoClose: Boolean) = {
        socketFactory.createSocket(socket, host, port, autoClose)
      }

      override def createSocket() =
        socketFactory.createSocket()
    }, 443))

    schemeRegistry
  }

  def mkHttp(keyStore: java.security.KeyStore) =
    new HttpProvider {
      val schemes = Http2.createSchemes(keyStore)
      val params = new org.apache.http.params.BasicHttpParams
      val client = new org.apache.http.impl.client.DefaultHttpClient(new org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager(params, schemes), params)
      client.setCookieStore(new org.apache.http.impl.client.BasicCookieStore)

      def apply[A](f: Http2[A]): Task[A] =
        Task.fork(
          \/.fromTryCatch(f(client)).fold(
            Task.fail(_),
            Task.now(_)
          )
        )
    }

  def post(url: String, params: List[(String, String)]): Http2[HttpResponse] = {
    val d = params.toList.map(x => new org.apache.http.message.BasicNameValuePair(x._1, x._2))
    val jlist = new java.util.ArrayList[org.apache.http.NameValuePair]
    d.foreach(jlist.add(_))

    val req = new methods.HttpPost(url)
    req.setEntity(new org.apache.http.client.entity.UrlEncodedFormEntity(jlist))

    http(req)
  }

  def post(uri: java.net.URI, params: PostQuery): Http2[HttpResponse] = {
    val p = params.foldLeft(new java.util.ArrayList[org.apache.http.NameValuePair]) { (a, x) =>
      a.add(new org.apache.http.message.BasicNameValuePair(x._1, x._2)); a }
    val r = new methods.HttpPost(uri)
    r.setEntity(new org.apache.http.client.entity.UrlEncodedFormEntity(p))
    http(r)
  }

  def head(url: String): Http2[HttpResponse] =
    http(new methods.HttpHead(url))

  @deprecated("Call with URI")
  def get(url: String): Http2[HttpResponse] =
    http(new methods.HttpGet(url))

  def get(uri: java.net.URI): Http2[HttpResponse] =
    http(new methods.HttpGet(uri))

  def getQuery(url: String, query: Query) =
    get(url + renderQuery(true, query))

  // TODO: remove this function, it's ugly. use InputStreamMaker
  def getContent(url: String): Http2[Option[String]] =
    get(url) flatMap (x => if (statusCode(x) == 200) Http2.point(contents(x)) else Http2.point(None))

  // This is not really a nice method. The use case is a single instance
  def getRange(uri: java.net.URI, start: Int, end: Option[Int]): Http2[HttpResponse] = {
    val r = new methods.HttpGet(uri)
    r.addHeader("Range", s"bytes=${start.shows}-${end.map(_.shows).getOrElse("")}")
    http(r)
  }

  def contentToStream(url: String, dest: java.io.OutputStream): Http2[Unit] =
    get(url).map(_.getEntity.writeTo(dest))

  def http(req: methods.HttpUriRequest): Http2[HttpResponse] =
    Http2(_ execute req)

  // Lift an `A` into the Http2 context, doing nothing with the provided client
  def point[A](a: => A): Http2[A] =
    Http2(_ => a)

  val headers =
    (h: HttpResponse) => List(h.getAllHeaders: _*)

  val contents =
    (h: HttpResponse) => \/.fromTryCatch {
      val in = h.getEntity.getContent
      val content = Stream.continually(in.read).takeWhile(_ != -1).map(_.toChar).mkString
      in.close
      content
    }.toOption

  val statusCode =
    (h: HttpResponse) => h.getStatusLine.getStatusCode

}

// specialized Reader for Http
case class Http2[A](g: HttpClient => A) {
  def apply(c: HttpClient) = g(c)

  def map[B](f: A => B): Http2[B] =
    Http2(c => f(g(c)))

  def flatMap[B](f: A => Http2[B]): Http2[B] =
    Http2(c => f(g(c))(c))
}

abstract class HttpProvider {
  def apply[A](f: Http2[A]): Task[A]
}
