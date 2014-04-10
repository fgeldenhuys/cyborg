package cyborg.net

import cyborg.net.Http._
import cyborg.util.binary._
import cyborg.util.execution._
import cyborg.util.execution.CancelledExecution
import cyborg.util.io._
import cyborg.util.control._
import java.io._
import java.net._
import javax.net.ssl.SSLSocketFactory
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.message.BasicNameValuePair
import scala.collection.JavaConversions._
import scala.concurrent._
import scala.concurrent.duration._

trait Http {
  import cyborg.Log._

  protected def createConnection(url: String)(implicit params: HttpParameters): Either[Throwable, HttpURLConnection]

  def get(url: String, data: Map[String, String] = Map.empty)(progress: Option[(Bytes) => Any])
         (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
         : Future[HttpResult] = future { blocking {
    val fullUrl = url + makeGetParams(data)
    $d(s"GET '$fullUrl'", 1)
    createConnection(fullUrl) match {
      case Left(t) => throw t
      case Right(http) =>
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
          case e @ (_:ConnectException | _:UnknownHostException | _:SocketTimeoutException) =>
            $d(s"${e.getClass} caught for '$url': $e")
            SimpleHttpResult(-1, e.getMessage)
          case e: FileNotFoundException =>
            val responseCode = http.getResponseCode
            val errorContent = read(http.getErrorStream)
            $w(s"$responseCode $e for '$fullUrl'")
            SimpleHttpResult(responseCode, errorContent)
          case e: IOException =>
            val responseCode = http.getResponseCode
            val errorContent = read(http.getErrorStream)
            $w(s"$responseCode $e for '$fullUrl'")
            SimpleHttpResult(responseCode, errorContent)
        }
    }
  }}

  def post(url: String, data: Map[String, String] = Map.empty)
          (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
          : Future[HttpResult] =
  {
    val p = promise[HttpResult]()
    future { blocking {
      $d(s"POST '$url' $data", 1)
      createConnection(url)(params.copy(chunked = false)) match {
        case Left(t) => throw t
        case Right(http) =>
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
            case e @ (_:ConnectException | _:UnknownHostException | _:SocketTimeoutException) =>
              $d(s"${e.getClass} caught for '$url': $e")
              p success SimpleHttpResult(-1, e.getMessage)
            case e: FileNotFoundException =>
              $d(s"${e.getClass} caught")
              execute {
                val responseCode = http.getResponseCode
                $w(s"$responseCode $e for '$url'")
                p success SimpleHttpResult(responseCode, "Resource not found")
              } within (5 seconds) recover {
                case CancelledExecution(message) =>
                  $w(s"Timeout getting error content for '$url'")
                  p success SimpleHttpResult(-1, message)
              }
            case e: IOException =>
              $d(s"${e.getClass} caught for '$url': $e")
              execute {
                val errorContent = read(http.getErrorStream)
                val responseCode = http.getResponseCode
                $w(s"$responseCode $e for '$url'")
                p success SimpleHttpResult(responseCode, errorContent)
              } within (5 seconds) recover {
                case CancelledExecution(message) =>
                  $w(s"Timeout getting error content for '$url'")
                  p success SimpleHttpResult(-1, message)
              }
          }
      }
    }}
    p.future
  }

  def postString(url: String, data: String, contentType: String)
         (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
         : Future[HttpResult] =
  {
    val p = promise[HttpResult]()
    future { blocking {
      $d(s"POST '$url' $data'", 1)
      createConnection(url)(params.copy(chunked = false)) match {
        case Left(t) => throw t
        case Right(http) =>
          try {
            val rawData = data.utf8
            http.setRequestMethod("POST")
            http.setRequestProperty("Content-Type", contentType)
            http.setDoOutput(true)
            http.setFixedLengthStreamingMode(rawData.size)
            val out = new BufferedOutputStream(http.getOutputStream)
            out.write(rawData)
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
            case e @ (_:ConnectException | _:UnknownHostException | _:SocketTimeoutException) =>
              $d(s"${e.getClass} caught for '$url': $e")
              p success SimpleHttpResult(-1, e.getMessage)
            case e: FileNotFoundException =>
              $d(s"${e.getClass} caught")
              execute {
                val responseCode = http.getResponseCode
                $w(s"$responseCode $e for '$url'")
                p success SimpleHttpResult(responseCode, "Resource not found")
              } within (5 seconds) recover {
                case CancelledExecution(message) =>
                  $w(s"Timeout getting error content for '$url'")
                  p success SimpleHttpResult(-1, message)
              }
            case e: IOException =>
              $d(s"${e.getClass} caught for '$url': $e")
              execute {
                val errorContent = read(http.getErrorStream)
                val responseCode = http.getResponseCode
                $w(s"$responseCode $e for '$url'")
                p success SimpleHttpResult(responseCode, errorContent)
              } within (5 seconds) recover {
                case CancelledExecution(message) =>
                  $w(s"Timeout getting error content for '$url'")
                  p success SimpleHttpResult(-1, message)
              }
          }
      }
    }}
    p.future
  }

  def put(url: String, data: String, contentType: String)
          (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
          : Future[HttpResult] =
  {
    val p = promise[HttpResult]()
    future { blocking {
      $d(s"PUT '$url' $data'", 1)
      createConnection(url)(params.copy(chunked = false)) match {
        case Left(t) => throw t
        case Right(http) =>
          try {
            val rawData = data.utf8
            http.setRequestMethod("POST")
            http.setRequestProperty("Content-Type", contentType)
            http.setDoOutput(true)
            http.setFixedLengthStreamingMode(rawData.size)
            val out = new BufferedOutputStream(http.getOutputStream)
            out.write(rawData)
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
            case e @ (_:ConnectException | _:UnknownHostException | _:SocketTimeoutException) =>
              $d(s"${e.getClass} caught for '$url': $e")
              p success SimpleHttpResult(-1, e.getMessage)
            case e: FileNotFoundException =>
              $d(s"${e.getClass} caught")
              execute {
                val responseCode = http.getResponseCode
                $w(s"$responseCode $e for '$url'")
                p success SimpleHttpResult(responseCode, "Resource not found")
              } within (5 seconds) recover {
                case CancelledExecution(message) =>
                  $w(s"Timeout getting error content for '$url'")
                  p success SimpleHttpResult(-1, message)
              }
            case e: IOException =>
              $d(s"${e.getClass} caught for '$url': $e")
              execute {
                val errorContent = read(http.getErrorStream)
                val responseCode = http.getResponseCode
                $w(s"$responseCode $e for '$url'")
                p success SimpleHttpResult(responseCode, errorContent)
              } within (5 seconds) recover {
                case CancelledExecution(message) =>
                  $w(s"Timeout getting error content for '$url'")
                  p success SimpleHttpResult(-1, message)
              }
          }
      }
    }}
    p.future
  }

  def delete(url: String)
            (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
            : Future[HttpResult] = future {
    $d(s"DELETE '$url'", 1)
    createConnection(url) match {
      case Left(t) => throw t
      case Right(http) =>
        try {
          http.setRequestMethod("DELETE")
          val content = read(http.getInputStream)
          val code = http.getResponseCode
          http.disconnect()
          SimpleHttpResult(code, content)
        }
        catch {
          case e @ (_:ConnectException | _:UnknownHostException | _:SocketTimeoutException) =>
            $d(s"${e.getClass} caught for '$url': $e")
            SimpleHttpResult(-1, e.getMessage)
          case e: FileNotFoundException =>
            val responseCode = http.getResponseCode
            $w(s"$responseCode $e for '$url'")
            SimpleHttpResult(responseCode, "Resource not found")
          case e: IOException =>
            val errorContent = read(http.getErrorStream)
            val responseCode = http.getResponseCode
            $w(s"$responseCode $e for '$url'")
            SimpleHttpResult(responseCode, errorContent)
        }
    }
  }

  def getFile(url: String, outputFile: String, data: Map[String, String] = Map.empty)
         (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
         : Future[HttpResult] = future { blocking {
    import cyborg.util.io._
    val fullUrl = url + makeGetParams(data)
    $d(s"GET FILE '$fullUrl'", 1)
    createConnection(fullUrl) match {
      case Left(t) => throw t
      case Right(http) =>
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
          } else ""//read(http.getErrorStream)
          http.disconnect()
          SimpleHttpResult(code, content)
        }
        catch {
          case e @ (_:ConnectException | _:UnknownHostException | _:SocketTimeoutException) =>
            $d(s"${e.getClass} caught for '$url': $e")
            SimpleHttpResult(-1, e.getMessage)
          case e: FileNotFoundException =>
            val responseCode = http.getResponseCode
            $w(s"$responseCode $e for '$fullUrl'")
            SimpleHttpResult(responseCode, "Resource not found")
          case e: IOException =>
            val errorContent = read(http.getErrorStream)
            val responseCode = http.getResponseCode
            $w(s"$responseCode $e for '$fullUrl'")
            SimpleHttpResult(responseCode, errorContent)
        }
    }
  }}

  def getFileM(url: String, outputFile: String, data: Map[String, String] = Map.empty)
              (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
              : Monitor[HttpResult, Bytes] = {
    import cyborg.util.io._
    val m = monitor[HttpResult, Bytes]
    future { blocking {
      val fullUrl = url + makeGetParams(data)
      $d(s"GET FILE '$fullUrl'", 1)
      createConnection(fullUrl) match {
        case Left(t) =>
          //$w("GET FILE createConnection failed: " + t)
          m failure t
        case Right(http) =>
          try {
            val in = http.getInputStream
            val code = http.getResponseCode
            val length = http.getContentLength
            if (length > -1) m withMaxProgress Bytes(length)
            if (code / 100 == 2) {
              val file = new java.io.File(outputFile)
              file.getCanonicalFile.getParentFile.mkdirs()
              file.createNewFile()
              val streamMonitor = inStream2NewFileM(in, file)
              m.track(streamMonitor) { (size: Int) =>
                http.disconnect()
                $d(s"Downloaded '${file.getAbsolutePath}' of size $size")
                SimpleHttpResult(code, file.getAbsolutePath)
              }
            }
            else
              m success SimpleHttpResult(code, "")
          }
          catch {
            case e @ (_:ConnectException | _:UnknownHostException | _:SocketTimeoutException) =>
              $d(s"${e.getClass} caught for '$url': $e")
              m success SimpleHttpResult(-1, e.getMessage)
            case e: FileNotFoundException =>
              val responseCode = http.getResponseCode
              $w(s"$responseCode $e for '$fullUrl'")
              m success SimpleHttpResult(responseCode, "Resource not found")
            case e: IOException =>
              val responseCode = http.getResponseCode
              $w(s"$responseCode $e for '$fullUrl'")
              m success SimpleHttpResult(responseCode, "")
          }
      }
    }}
    m
  }

  def getBytes(url: String, data: Map[String, String] = Map.empty)(progress: Option[(Bytes) => Any])
         (implicit params: HttpParameters = defaultHttpParameters, sec: ScheduledExecutionContext)
  : Future[Array[Byte]] = future { blocking {
    val fullUrl = url + makeGetParams(data)
    $d(s"GET BYTES '$fullUrl'", 1)
    createConnection(fullUrl) match {
      case Left(t) => throw t
      case Right(http) =>
        val inputStream = http.getInputStream
        val bytes = new ByteArrayOutputStream()
        if (progress.isDefined)
          inStream2outStreamWithProgress(inputStream, bytes)(progress.get)
        else
          inStream2outStream(inputStream, bytes)
        http.disconnect()
        bytes.toByteArray
    }
  }}

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

    protected def createConnection(url: String)(implicit params: HttpParameters) =
      http.createConnection(addHost(url))
    override def get(url: String, data: Map[String, String])
                    (progress: Option[(Bytes) => Any])
                    (implicit params: HttpParameters, sec: ScheduledExecutionContext): Future[HttpResult] =
      http.get(addHost(url), data)(progress)(params, sec)
    override def post(url: String, data: Map[String, String])
                     (implicit params: HttpParameters, sec: ScheduledExecutionContext): Future[HttpResult] =
      http.post(addHost(url), data)(params, sec)
    override def postString(url: String, data: String, contentType: String)
                     (implicit params: HttpParameters, sec: ScheduledExecutionContext): Future[HttpResult] =
      http.postString(addHost(url), data, contentType)(params, sec)
    override def put(url: String, data: String, contentType: String)
                     (implicit params: HttpParameters, sec: ScheduledExecutionContext): Future[HttpResult] =
      http.put(addHost(url), data, contentType)(params, sec)
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

  trait HttpResult extends Throwable {
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

  def makeGetParams(params: Map[String, String]): String = {
    if (params.isEmpty) ""
    else "?" +
      URLEncodedUtils.format((params map { case (k: String,v: String) =>
        new BasicNameValuePair(k, v) }).toList, "utf-8")
  }

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
