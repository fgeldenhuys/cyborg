package cyborg.net

import java.io.{BufferedOutputStream, InputStream, IOException, FileNotFoundException}
import java.net.{HttpURLConnection, URL}
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.message.BasicNameValuePair
import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.concurrent.{future, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Http {
  case class HttpParameters(
    connectTimeout: Duration,
    readTimeout: Duration,
    chunked: Boolean
  )
  implicit val defaultHttpParameters = HttpParameters(
    connectTimeout = (5 seconds),
    readTimeout = (5 seconds),
    chunked = true
  )

  case class HttpResult(code: Int, content: String) {
    def success: Boolean = code / 100 == 2
    def redirect: Boolean = code / 100 == 3
    def error: Boolean = code / 100 == 4
    def serverError: Boolean = code / 100 == 5
  }

  private def read(in: InputStream) =
    new String(Stream.continually(in.read).takeWhile(_ != -1).map(_.toByte).toArray)

  private def makeGetParams(params: Map[String, String]): String =
    URLEncodedUtils.format((params map { case (k: String,v: String) =>
      new BasicNameValuePair(k, v) }).toList, "utf-8")

  private def makeFormParams(params: Map[String, String]): UrlEncodedFormEntity = {
    new UrlEncodedFormEntity(
      (params map { case (k: String,v: String) =>
        new BasicNameValuePair(k, v) }).toList)
  }

  def get(url: String, data: Map[String, String] = Map.empty)
         (implicit params: HttpParameters = defaultHttpParameters): Future[HttpResult] = future {
    val fullUrl = url + "?" + makeGetParams(data)
    val http = new URL(fullUrl).openConnection.asInstanceOf[HttpURLConnection]
    if (params.chunked) http.setChunkedStreamingMode(0)
    http.setRequestProperty("Accept-Encoding", "identity")
    http.setConnectTimeout(params.connectTimeout.toMillis.toInt)
    http.setReadTimeout(params.readTimeout.toMillis.toInt)
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
    val http = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
    if (params.chunked) http.setChunkedStreamingMode(0)
    http.setRequestProperty("Accept-Encoding", "identity")
    http.setConnectTimeout(params.connectTimeout.toMillis.toInt)
    http.setReadTimeout(params.readTimeout.toMillis.toInt)
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