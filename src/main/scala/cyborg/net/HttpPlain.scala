package cyborg.net

import java.net.{HttpURLConnection, URL}
import scala.util.control.Exception._

object HttpPlain extends Http {
  import Http._

  protected def createConnection(url: String)(implicit params: HttpParameters): Either[Throwable, HttpURLConnection] = {
    import cyborg.Log._
    System.setProperty("http.keepAlive", "false")
    catching[HttpURLConnection](classOf[Exception]) either {
      val urlObject = new URL(url)
      val urlConnection = urlObject.openConnection
      val http = urlConnection.asInstanceOf[HttpURLConnection]
      http.setRequestProperty("Accept-Encoding", "identity")
      http.setConnectTimeout(params.connectTimeout.toMillis.toInt)
      http.setReadTimeout(params.readTimeout.toMillis.toInt)
      if (params.chunked) http.setChunkedStreamingMode(0)
      http
    }
  }

}
