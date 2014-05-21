package cyborg.net

import cyborg.Context._
import cyborg.Log._
import cyborg.util.binary._
import java.net.URL
import java.security.cert.{CertificateFactory, X509Certificate}
import java.security.{GeneralSecurityException, KeyStore}
import javax.net.ssl._
import org.apache.http.conn.ssl.AllowAllHostnameVerifier
import scala.util.control.Exception._

object HttpSecure extends Http {
  import Http._

  case class WrongCertificateAuthority() extends GeneralSecurityException("Wrong certificate authority")

  protected def createConnection(url: String)(implicit params: HttpParameters): Either[Throwable, HttpsURLConnection] = {
    System.setProperty("http.keepAlive", "false")
    catching(classOf[Exception]) either {
      val http = new URL(url).openConnection.asInstanceOf[HttpsURLConnection]
      http.setRequestProperty("Accept-Encoding", "identity")
      http.setRequestProperty("Connection", "Close")
      http.setConnectTimeout(params.connectTimeout.toMillis.toInt)
      http.setReadTimeout(params.readTimeout.toMillis.toInt)
      if (params.chunked) http.setChunkedStreamingMode(0)
      http.setHostnameVerifier(new AllowAllHostnameVerifier())
      params.socketFactory.map(http setSSLSocketFactory _)
      http
    }
  }

  def keyStore(keyStoreRes: Int, password: String)(implicit context: Context) = {
    val keyStore = KeyStore.getInstance("BKS")
    val in = context.resources.openRawResource(keyStoreRes)
    keyStore.load(in, password.toCharArray)
    in.close()
    keyStore
  }

  def makeSslSocketFactoryFromKeyStore(keyStore: KeyStore): SSLSocketFactory = {
    val tmf = TrustManagerFactory.getInstance("X509")
    tmf.init(keyStore)
    val ssl = SSLContext.getInstance("TLS")
    ssl.init(null, tmf.getTrustManagers, null)
    ssl.getSocketFactory
  }

  def makeSslSocketFactoryFromTrustManager(tm: TrustManager): SSLSocketFactory = {
    val ssl = SSLContext.getInstance("TLS")
    ssl.init(null, Array(tm), null)
    ssl.getSocketFactory
  }

  def makeX509TrustManager(pemResource: Int)
                          (implicit context: Context): X509TrustManager = {
    new X509TrustManager {
      def getAcceptedIssuers: Array[X509Certificate] = null
      def checkClientTrusted(certs: Array[X509Certificate], authType: String) {}
      def checkServerTrusted(certs: Array[X509Certificate], authType: String) {
        val in = context.resources.openRawResource(pemResource)
        val cf = CertificateFactory.getInstance("X.509")
        val ca = cf.generateCertificate(in).asInstanceOf[X509Certificate]
        in.close()
        val knownPK = ca.getPublicKey.getEncoded
        //$d(s"${certs.size} to verify")
        for (cert <- certs) {
          //$d(s"Verifying this from server: $cert")
          val serverPK = cert.getPublicKey.getEncoded
          //$d("Server's PK: " + serverPK.base64)
          //$d(s"Against this known certificate: $ca")
          //$d("   Known PK: " + knownPK.base64)
          if (!(serverPK sameBytesAs knownPK)) {
            $w("Server PK does not match known PK!")
            throw WrongCertificateAuthority()
          }
          //else
          //  $d("Server PK == Known PK. Everything is peachy")
          /*try {
            cert.verify(ca.getPublicKey)
          }
          catch {
            case e: Exception =>
              $d("verify: " + e.toString)
              throw e
          }*/
        }
      }
    }
  }
}
