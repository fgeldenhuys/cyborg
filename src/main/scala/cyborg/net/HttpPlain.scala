package cyborg.net

import java.net.{HttpURLConnection, URL}
import cyborg.util.execution.ScheduledExecutionContext
import cyborg.util.debug
import scala.util.control.Exception._

object HttpPlain extends Http {
  import Http._

  protected def createConnection(url: String)
                                (implicit params: HttpParameters, sec: ScheduledExecutionContext): Either[Throwable, HttpURLConnection] = {
    import cyborg.Log._
    System.setProperty("http.keepAlive", "false")
    catching[HttpURLConnection](classOf[Exception]) either {
      debug.warnAfterTime(1000) {
        val urlObject = new URL(url)
        val urlConnection = urlObject.openConnection
        val http = urlConnection.asInstanceOf[HttpURLConnection]
        http.setRequestProperty("Accept-Encoding", "identity")
        // FUCKING KITKAT!!!!!!11   The next line actually stops keep-alive, which on KitKat (and who knows where
        // else), leaks open file handles. This then causes all kinds of other things to fail randomly.
        http.setRequestProperty("Connection", "Close")
        http.setConnectTimeout(params.connectTimeout.toMillis.toInt)
        http.setReadTimeout(params.readTimeout.toMillis.toInt)
        if (params.chunked) http.setChunkedStreamingMode(0)
        http
      }
    }
  }

}
