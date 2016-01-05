package cyborg.net

import scalaz._, Scalaz._

import scala.collection.JavaConversions._

import java.io.InputStream
import java.security.KeyStore
import java.security.SecureRandom
import java.security.cert.CertificateFactory

import javax.net.ssl.SSLContext
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.TrustManagerFactory

import cyborg.util.scalazext._

object SSL {
  def emptyKeyStore(password: Array[Char]): Throwable \/ KeyStore = {
    \/.fromTryCatch {
      val keyStore = KeyStore.getInstance(KeyStore.getDefaultType)
      keyStore.load(null, password)
      keyStore
    }
  }

  def sslContextForTrustedCertificates(in: InputStream): Throwable \/ SSLContext = {
    \/.fromTryCatch {
      val certFactory = CertificateFactory.getInstance("X.509")
      val certificates = certFactory.generateCertificates(in)
      if (certificates.isEmpty)
        throw new IllegalArgumentException("Expected non-empty set of trusted certificates")
      val password = "password".toCharArray
      // Put certificates in a key store
      val keyStore = emptyKeyStore(password).getOrThrow
      for ((c, i) <- certificates.zipWithIndex) {
        keyStore.setCertificateEntry(i.toString, c)
      }
      // Wrap in SSL context
      val kmFactory = KeyManagerFactory
        .getInstance(KeyManagerFactory.getDefaultAlgorithm)
      kmFactory.init(keyStore, password)
      val tmFactory = TrustManagerFactory
        .getInstance(TrustManagerFactory.getDefaultAlgorithm)
      tmFactory.init(keyStore)
      val sslContext = SSLContext.getInstance("TLS")
      sslContext.init(kmFactory.getKeyManagers, tmFactory.getTrustManagers,
        new SecureRandom())
      sslContext
    }
  }

  def unsafeSslContext: Throwable \/ SSLContext = {
    \/.fromTryCatch {
      val tm = new javax.net.ssl.X509TrustManager {
        override def checkClientTrusted(
          chain: Array[java.security.cert.X509Certificate],
          authType: String): Unit = ()
        override def checkServerTrusted(
          chain: Array[java.security.cert.X509Certificate],
          authType: String): Unit = ()
        override def getAcceptedIssuers():
            Array[java.security.cert.X509Certificate] = null
      }
      val sslContext = SSLContext.getInstance("TLS")
      sslContext.init(null, Array(tm), new SecureRandom())
      sslContext
    }
  }

  def unsafeHostnameVerifier = new javax.net.ssl.HostnameVerifier {
    override def verify(hostname: String,
      session: javax.net.ssl.SSLSession) = true
  }
}
