package cyborg.net

import cyborg.net.Http._
import cyborg.net.Http.HttpParameters
import cyborg.net.Http.SimpleHttpResult
import java.io.{BufferedOutputStream, IOException, FileNotFoundException, InputStream}
import java.net.{CookieHandler, CookieManager, HttpURLConnection}
import javax.net.ssl.SSLSocketFactory
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.message.BasicNameValuePair
import scala.collection.JavaConversions._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait Http {
  import cyborg.Log._

  protected def createConnection(url: String)(implicit params: HttpParameters): HttpURLConnection

  def get(url: String, data: Map[String, String] = Map.empty)
         (implicit params: HttpParameters = defaultHttpParameters): Future[HttpResult] = future {
    $d(s"get url=$url")
    val http = createConnection(url + "?" + makeGetParams(data))
    try {
      val content = read(http.getInputStream)
      val code = http.getResponseCode
      http.disconnect()
      SimpleHttpResult(code, content)
    }
    catch {
      case e: FileNotFoundException =>
        val errorContent = read(http.getErrorStream)
        SimpleHttpResult(http.getResponseCode, errorContent)
      case e: IOException =>
        val errorContent = read(http.getErrorStream)
        SimpleHttpResult(http.getResponseCode, errorContent)
    }
  }

  def post(url: String, data: Map[String, String] = Map.empty)
          (implicit params: HttpParameters = defaultHttpParameters): Future[HttpResult] = future {
    val http = createConnection(url)(params.copy(chunked = false))
    try {
      http.setRequestMethod("POST")
      http.setDoOutput(true)
      val formEntity = makeFormParams(data)
      http.setFixedLengthStreamingMode(formEntity.getContentLength.toInt)
      val out = new BufferedOutputStream(http.getOutputStream)
      formEntity.writeTo(out)
      out.close()
      val content = read(http.getInputStream)
      val code = http.getResponseCode
      http.disconnect()
      SimpleHttpResult(code, content)
    }
    catch {
      case e: FileNotFoundException =>
        val errorContent = read(http.getErrorStream)
        SimpleHttpResult(http.getResponseCode, errorContent)
      case e: IOException =>
        val errorContent = read(http.getErrorStream)
        SimpleHttpResult(http.getResponseCode, errorContent)
    }
  }

  def delete(url: String)(implicit params: HttpParameters = defaultHttpParameters): Future[HttpResult] = future {
    val http = createConnection(url)
    try {
      http.setRequestMethod("DELETE")
      val content = read(http.getInputStream)
      val code = http.getResponseCode
      http.disconnect()
      SimpleHttpResult(code, content)
    }
    catch {
      case e: FileNotFoundException =>
        val errorContent = read(http.getErrorStream)
        SimpleHttpResult(http.getResponseCode, errorContent)
      case e: IOException =>
        val errorContent = read(http.getErrorStream)
        SimpleHttpResult(http.getResponseCode, errorContent)
    }
  }

  def getFile(url: String, outputFile: String, data: Map[String, String] = Map.empty)
         (implicit params: HttpParameters = defaultHttpParameters): Future[HttpResult] = future {
    import cyborg.util.io._
    val http = createConnection(url + "?" + makeGetParams(data))
    try {
      val in = http.getInputStream
      val code = http.getResponseCode
      val content = if (code / 100 == 2) {
        val file = new java.io.File(outputFile)
        file.getCanonicalFile.getParentFile.mkdirs()
        file.createNewFile()
        inStream2NewFile(in, file)
        file.getAbsolutePath
      } else read(http.getErrorStream)
      http.disconnect()
      SimpleHttpResult(code, content)
    }
    catch {
      case e: FileNotFoundException =>
        val errorContent = read(http.getErrorStream)
        SimpleHttpResult(http.getResponseCode, errorContent)
      case e: IOException =>
        val errorContent = read(http.getErrorStream)
        SimpleHttpResult(http.getResponseCode, errorContent)
    }
  }

  def withHost(host: String) = new HttpHostWrapper(this, host)
}

object Http {
  val cookieManager = new CookieManager()
  CookieHandler.setDefault(cookieManager)

  class HttpHostWrapper(val http: Http, hostPrefix: String) extends Http {
    val host = if (hostPrefix.endsWith("/"))
      hostPrefix.substring(0, hostPrefix.length-1)
    else
      hostPrefix

    protected def addHost(path: String) =
      if (path.startsWith("/")) host + path
      else host + "/" + path

    protected def createConnection(url: String)(implicit params: HttpParameters): HttpURLConnection =
      http.createConnection(addHost(url))
    override def get(url: String, data: Map[String, String])(implicit params: HttpParameters): Future[HttpResult] =
      http.get(addHost(url), data)(params)
    override def post(url: String, data: Map[String, String])(implicit params: HttpParameters): Future[HttpResult] =
      http.post(addHost(url), data)(params)
    override def getFile(url: String, outputFile: String, data: Map[String, String])
                        (implicit params: HttpParameters): Future[HttpResult] =
      http.getFile(addHost(url), outputFile, data)(params)
  }

  case class HttpParameters(
    connectTimeout: Duration,
    readTimeout: Duration,
    chunked: Boolean,
    socketFactory: Option[SSLSocketFactory]
  )

  val defaultHttpParameters = HttpParameters(
    connectTimeout = (5 seconds),
    readTimeout = (5 seconds),
    chunked = true,
    socketFactory = None
  )

  trait HttpResult {
    def code: Int
    def content: String

    def success: Boolean = code / 100 == 2
    def redirect: Boolean = code / 100 == 3
    def error: Boolean = code / 100 == 4
    def serverError: Boolean = code / 100 == 5
  }

  case class SimpleHttpResult(code: Int, content: String) extends HttpResult

  object HttpSuccessResult {
    def unapply(result: SimpleHttpResult): Option[(Int, String)] =
      if (result.success) Some(result.code, result.content) else None
  }
  object HttpFailedResult {
    def unapply(result: SimpleHttpResult): Option[(Int, String)] =
      if (!result.success) Some(result.code, result.content) else None
  }

  def read(in: InputStream) = Option(in) map { (in) =>
    new String(Stream.continually(in.read).takeWhile(_ != -1).map(_.toByte).toArray)
  } getOrElse ("")

  def makeGetParams(params: Map[String, String]): String =
    URLEncodedUtils.format((params map { case (k: String,v: String) =>
      new BasicNameValuePair(k, v) }).toList, "utf-8")

  def makeFormParams(params: Map[String, String]): UrlEncodedFormEntity = {
    new UrlEncodedFormEntity(
      (params map { case (k: String,v: String) =>
        new BasicNameValuePair(k, v) }).toList)
  }
}