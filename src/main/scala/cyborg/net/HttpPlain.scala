package cyborg.net

import java.net.{HttpURLConnection, URL}
import scala.util.control.Exception._

object HttpPlain extends Http {
  import Http._

  protected def createConnection(url: String)(implicit params: HttpParameters): Either[Throwable, HttpURLConnection] = {
    import cyborg.Log._
    System.setProperty("http.keepAlive", "false")
    catching[HttpURLConnection](classOf[Exception]) either {
      $d("cc")
      val urlObject = new URL(url)
      $d("urlObject="+urlObject)
      val urlConnection = urlObject.openConnection
      $d("urlConnection="+urlConnection)
      val http = urlConnection.asInstanceOf[HttpURLConnection]
      $d("http="+http)
      http.setRequestProperty("Accept-Encoding", "identity")
      http.setConnectTimeout(params.connectTimeout.toMillis.toInt)
      http.setReadTimeout(params.readTimeout.toMillis.toInt)
      if (params.chunked) http.setChunkedStreamingMode(0)
      http
    }
  }

}
