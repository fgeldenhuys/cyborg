package cyborg.net

import java.net.URL
import javax.net.ssl.{HttpsURLConnection, SSLContext, TrustManagerFactory, SSLSocketFactory}
import java.security.KeyStore
import cyborg.Context
import org.apache.http.conn.ssl.AllowAllHostnameVerifier

object HttpSecure extends Http {
  import Http._

  protected def createConnection(url: String)(implicit params: HttpParameters): HttpsURLConnection = {
    val http = new URL(url).openConnection.asInstanceOf[HttpsURLConnection]
    http.setRequestProperty("Accept-Encoding", "identity")
    http.setConnectTimeout(params.connectTimeout.toMillis.toInt)
    http.setReadTimeout(params.readTimeout.toMillis.toInt)
    if (params.chunked) http.setChunkedStreamingMode(0)
    http.setHostnameVerifier(new AllowAllHostnameVerifier())
    params.socketFactory.map(http setSSLSocketFactory _)
    http
  }

  def makeSslSocketFactory(keyStoreRes: Int, password: String)
                          (implicit context: Context): SSLSocketFactory = {
    val keyStore = KeyStore.getInstance("BKS")
    val in = context.getResources.openRawResource(keyStoreRes)
    keyStore.load(in, password.toCharArray)
    in.close()
    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(keyStore)
    val ssl = SSLContext.getInstance("TLS")
    ssl.init(null, tmf.getTrustManagers, null)
    ssl.getSocketFactory
  }

}
