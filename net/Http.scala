package cyborg.net

import scala.concurrent.duration._
import java.io.{BufferedOutputStream, IOException, FileNotFoundException, InputStream}
import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.message.BasicNameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import scala.collection.JavaConversions._
import cyborg.net.Http._
import concurrent._
import cyborg.net.Http.HttpParameters
import cyborg.net.Http.HttpResult
import java.net.HttpURLConnection
import javax.net.ssl.SSLSocketFactory

trait Http {
  protected def createConnection(url: String)(implicit params: HttpParameters): HttpURLConnection

  def get(url: String, data: Map[String, String] = Map.empty)
         (implicit params: HttpParameters = defaultHttpParameters): Future[HttpResult] = future {
    val http = createConnection(url + "?" + makeGetParams(data))
    try {
      val content = read(http.getInputStream)
      val code = http.getResponseCode
      http.disconnect()
      HttpResult(code, content)
    }
    catch {
      case e: FileNotFoundException =>
        val errorContent = read(http.getErrorStream)
        HttpResult(http.getResponseCode, errorContent)
      case e: IOException =>
        val errorContent = read(http.getErrorStream)
        HttpResult(http.getResponseCode, errorContent)
    }
  }

  def post(url: String, data: Map[String, String] = Map.empty)
          (implicit params: HttpParameters = defaultHttpParameters): Future[HttpResult] = future {
    val http = createConnection(url)
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
      HttpResult(code, content)
    }
    catch {
      case e: FileNotFoundException =>
        val errorContent = read(http.getErrorStream)
        HttpResult(http.getResponseCode, errorContent)
      case e: IOException =>
        val errorContent = read(http.getErrorStream)
        HttpResult(http.getResponseCode, errorContent)
    }
  }

}

object Http {
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

  case class HttpResult(code: Int, content: String) {
    def success: Boolean = code / 100 == 2
    def redirect: Boolean = code / 100 == 3
    def error: Boolean = code / 100 == 4
    def serverError: Boolean = code / 100 == 5
  }

  def read(in: InputStream) =
    new String(Stream.continually(in.read).takeWhile(_ != -1).map(_.toByte).toArray)

  def makeGetParams(params: Map[String, String]): String =
    URLEncodedUtils.format((params map { case (k: String,v: String) =>
      new BasicNameValuePair(k, v) }).toList, "utf-8")

  def makeFormParams(params: Map[String, String]): UrlEncodedFormEntity = {
    new UrlEncodedFormEntity(
      (params map { case (k: String,v: String) =>
        new BasicNameValuePair(k, v) }).toList)
  }
}
