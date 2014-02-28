package cyborg.crypto

import scalaz._, Scalaz._
import cyborg.util.binary._
import javax.crypto.spec.IvParameterSpec

object SimpleEncryption {
  trait SimpleEncryption[A] {
    def encrypt(input: A, key: CryptoKey): Array[Byte]
    def decrypt(input: Array[Byte], key: CryptoKey): A
  }

  def encrypt[A](input: A, key: CryptoKey)(implicit method: SimpleEncryption[A]): Array[Byte] =
    method.encrypt(input, key)

  def decrypt[A](input: Array[Byte], key: CryptoKey)(implicit method: SimpleEncryption[A]): A =
    method.decrypt(input, key)

  implicit val byteArraySimpleEncryption = new SimpleEncryption[Array[Byte]] {
    def encrypt(input: Array[Byte], key: CryptoKey): Array[Byte] = {
      val cipher = key.makeEncryptCipher
      val iv = cipher.getIV
      val output = cipher.doFinal(input)
      arrayByteBuffer(1 + iv.size + output.size) << iv.size.toByte << iv << output array()
    }

    def decrypt(input: Array[Byte], key: CryptoKey): Array[Byte] = {
      val ivSize = input(0).toInt
      val ivSpec = new IvParameterSpec(input, 1, ivSize)
      val cipher = key.makeDecryptCipher(ivSpec)
      cipher.doFinal(input, 1 + ivSize, input.size - 1 - ivSize)
    }
  }

  implicit val stringSimpleEncryption = new SimpleEncryption[String] {
    def encrypt(input: String, key: CryptoKey) =
      byteArraySimpleEncryption.encrypt(input.getBytes("UTF-8"), key)
    def decrypt(input: Array[Byte], key: CryptoKey) =
      new String(byteArraySimpleEncryption.decrypt(input, key), "UTF-8")
  }
}
