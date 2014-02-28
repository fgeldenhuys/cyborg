package cyborg.crypto

import cyborg.util.binary._
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.Cipher

case class CryptoKey(key: Array[Byte], algorithm: String, transformation: String) {
  lazy val secretKeySpec = new SecretKeySpec(key, algorithm)

  def makeEncryptCipher = {
    val result = Cipher.getInstance(transformation)
    result.init(Cipher.ENCRYPT_MODE, secretKeySpec)
    result
  }

  def makeDecryptCipher(ivSpec: IvParameterSpec) = {
    val result = Cipher.getInstance(transformation)
    result.init(Cipher.DECRYPT_MODE, secretKeySpec, ivSpec)
    result
  }
}

object CryptoKey {
  def fromBase64(base64: String, algorithm: String, transformation: String): CryptoKey =
    CryptoKey(base64.decodeBase64, algorithm, transformation)
}
