package cyborg.net

import java.net.{HttpURLConnection, URL}

object HttpPlain extends Http {
  import Http._

  protected def createConnection(url: String)(implicit params: HttpParameters): HttpURLConnection = {
    val http = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
    http.setRequestProperty("Accept-Encoding", "identity")
    http.setConnectTimeout(params.connectTimeout.toMillis.toInt)
    http.setReadTimeout(params.readTimeout.toMillis.toInt)
    if (params.chunked) http.setChunkedStreamingMode(0)
    http
  }

}
