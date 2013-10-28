package cyborg.net

import cyborg.net.Http._
import cyborg.net.Http.HttpParameters
import cyborg.net.Http.SimpleHttpResult
import cyborg.util.binary._
import cyborg.util.execution._
import cyborg.util.io._
import java.io._
import java.net.{URLEncoder, CookieHandler, CookieManager, HttpURLConnection}
import javax.net.ssl.SSLSocketFactory
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.message.BasicNameValuePair
import scala.collection.JavaConversions._
import scala.concurrent._
import scala.concurrent.duration._
import scala.Some

trait Http {
  import cyborg.Log._

  protected def createConnection(url: String)(implicit params: HttpParameters): HttpURLConnection

  def get(url: String, data: Map[String, String] = Map.empty)(progress: Option[(Bytes) => Any])
         (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
         : Future[HttpResult] = future {
    val getParams = makeGetParams(data)
    val fullUrl = url + (if (getParams.isEmpty) "" else "?" + getParams)
    $d(s"GET '$fullUrl'", 1)
    val http = createConnection(fullUrl)
    try {
      val inputStream = http.getInputStream
      val content =
        if (progress.isDefined) readWithProgress(inputStream)(progress.get)
        else read(inputStream)
      val code = http.getResponseCode
      http.disconnect()
      SimpleHttpResult(code, content)
    }
    catch {
      case e: FileNotFoundException =>
        val errorContent = read(http.getErrorStream)
        val responseCode = http.getResponseCode
        $w(s"$responseCode $e for '$fullUrl'")
        SimpleHttpResult(responseCode, errorContent)
      case e: IOException =>
        val errorContent = read(http.getErrorStream)
        val responseCode = http.getResponseCode
        $w(s"$responseCode $e for '$fullUrl'")
        SimpleHttpResult(responseCode, errorContent)
    }
  }

  def post(url: String, data: Map[String, String] = Map.empty)
          (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
          : Future[HttpResult] =
  {
    val p = promise[HttpResult]()
    future {
      $d(s"POST '$url' $data", 1)
      val http = createConnection(url)(params.copy(chunked = false))
      try {
        http.setRequestMethod("POST")
        http.setDoOutput(true)
        val formEntity = makeFormParams(data)
        http.setFixedLengthStreamingMode(formEntity.getContentLength.toInt)
        val out = new BufferedOutputStream(http.getOutputStream)
        formEntity.writeTo(out)
        out.close()
        //$d(s"[Http.post] Reading content.")
        val content = read(http.getInputStream)
        //$d(s"[Http.post] Content is ${content.length} bytes. Getting response code.")
        val code = http.getResponseCode
        //$d(s"[Http.post] Response code was $code.")
        http.disconnect()
        p success SimpleHttpResult(code, content)
      }
      catch {
        case e: FileNotFoundException =>
          $d("FileNotFoundException caught")
          execute {
            val errorContent = read(http.getErrorStream)
            val responseCode = http.getResponseCode
            $w(s"$responseCode $e for '$url'")
            p success SimpleHttpResult(responseCode, errorContent)
          } within (10 seconds) recover {
            case CancelledExecution(message) =>
              $w(s"Timeout error for '$url'")
              p success SimpleHttpResult(-1, message)
          }
        case e: IOException =>
          $d(s"IOException caught for '$url'")
          execute {
            val errorContent = read(http.getErrorStream)
            val responseCode = http.getResponseCode
            $w(s"$responseCode $e for '$url'")
            p success SimpleHttpResult(responseCode, errorContent)
          } within (10 seconds) recover {
            case CancelledExecution(message) =>
              $w(s"Timeout error for '$url'")
              p success SimpleHttpResult(-1, message)
          }
      }
    }
    p.future
  }

  def delete(url: String)
            (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
            : Future[HttpResult] = future {
    $d(s"DELETE '$url'", 1)
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
        val responseCode = http.getResponseCode
        $w(s"$responseCode $e for '$url'")
        SimpleHttpResult(responseCode, errorContent)
      case e: IOException =>
        val errorContent = read(http.getErrorStream)
        val responseCode = http.getResponseCode
        $w(s"$responseCode $e for '$url'")
        SimpleHttpResult(responseCode, errorContent)
    }
  }

  def getFile(url: String, outputFile: String, data: Map[String, String] = Map.empty)
         (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
         : Future[HttpResult] = future {
    import cyborg.util.io._
    val fullUrl = url + "?" + makeGetParams(data)
    $d(s"GET FILE '$fullUrl'", 1)
    val http = createConnection(fullUrl)
    try {
      val in = http.getInputStream
      val code = http.getResponseCode
      val content = if (code / 100 == 2) {
        val file = new java.io.File(outputFile)
        file.getCanonicalFile.getParentFile.mkdirs()
        file.createNewFile()
        val size = inStream2NewFile(in, file)
        $d(s"Downloaded '${file.getAbsolutePath}' of size $size")
        file.getAbsolutePath
      } else read(http.getErrorStream)
      http.disconnect()
      SimpleHttpResult(code, content)
    }
    catch {
      case e: FileNotFoundException =>
        val errorContent = read(http.getErrorStream)
        val responseCode = http.getResponseCode
        $w(s"$responseCode $e for '$fullUrl'")
        SimpleHttpResult(responseCode, errorContent)
      case e: IOException =>
        val errorContent = read(http.getErrorStream)
        val responseCode = http.getResponseCode
        $w(s"$responseCode $e for '$fullUrl'")
        SimpleHttpResult(responseCode, errorContent)
    }
  }

  def getBytes(url: String, data: Map[String, String] = Map.empty)(progress: Option[(Bytes) => Any])
         (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
  : Future[Array[Byte]] = future {
    val getParams = makeGetParams(data)
    val fullUrl = url + (if (getParams.isEmpty) "" else "?" + getParams)
    $d(s"GET BYTES '$fullUrl'", 1)
    val http = createConnection(fullUrl)
    val inputStream = http.getInputStream
    val bytes = new ByteArrayOutputStream()
    if (progress.isDefined)
      inStream2outStreamWithProgress(inputStream, bytes)(progress.get)
    else
      inStream2outStream(inputStream, bytes)
    http.disconnect()
    bytes.toByteArray
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
    override def get(url: String, data: Map[String, String])
                    (progress: Option[(Bytes) => Any])
                    (implicit params: HttpParameters, sec: ScheduledExecutionContext): Future[HttpResult] =
      http.get(addHost(url), data)(progress)(params, sec)
    override def post(url: String, data: Map[String, String])
                     (implicit params: HttpParameters, sec: ScheduledExecutionContext): Future[HttpResult] =
      http.post(addHost(url), data)(params, sec)
    override def getFile(url: String, outputFile: String, data: Map[String, String])
                        (implicit params: HttpParameters, sec: ScheduledExecutionContext): Future[HttpResult] =
      http.getFile(addHost(url), outputFile, data)(params, sec)
  }

  case class HttpParameters(
    connectTimeout: Duration,
    readTimeout: Duration,
    chunked: Boolean,
    socketFactory: Option[SSLSocketFactory]
  )

  val defaultHttpParameters = HttpParameters(
    connectTimeout = 5 seconds,
    readTimeout = 5 seconds,
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

  def readWithProgress(in: InputStream)(f: (Bytes) => Any) = Option(in) map { in =>
    val result = new ByteArrayOutputStream()
    inStream2outStreamWithProgress(in, result)(f)
    result.toString("UTF-8")
  } getOrElse ("")

  def makeGetParams(params: Map[String, String]): String =
    URLEncodedUtils.format((params map { case (k: String,v: String) =>
      new BasicNameValuePair(k, v) }).toList, "utf-8")

  def makeFormParams(params: Map[String, String]): UrlEncodedFormEntity = {
    new UrlEncodedFormEntity(
      (params map { case (k: String,v: String) =>
        new BasicNameValuePair(k, v) }).toList)
  }

  def getContent(result: HttpResult): Option[String] = {
    result match {
      case HttpSuccessResult(_, content) => Some(content)
      case _ => None
    }
  }
}
