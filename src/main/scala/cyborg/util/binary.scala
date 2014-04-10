package cyborg.util

import java.nio.ByteBuffer
import scala.util.control.Exception._
import java.io.ByteArrayOutputStream
import android.util.Base64
import java.security.MessageDigest

object binary {
  object Bytes {
    val KiB = 1024
    val MiB = KiB * 1024
    val GiB = MiB * 1024
    val TiB = GiB * 1024
  }
  case class Bytes(bytes: Int) extends AnyVal {
    import Bytes._
    override def toString = {
      if (bytes < KiB) s"$bytes bytes"
      else if (bytes < MiB) f"${bytes.toFloat / KiB.toFloat}%.3f KiB"
      else if (bytes < GiB) f"${bytes.toFloat / MiB.toFloat}%.3f MiB"
      else if (bytes < TiB) f"${bytes.toFloat / GiB.toFloat}%.3f GiB"
      else f"${bytes.toFloat / TiB.toFloat}%.3f TiB"
    }

    def toInt = bytes
    def toDouble = bytes.toDouble

    def == (that: Bytes) = bytes == that.bytes
    def < (that: Bytes) = bytes < that.bytes
    def > (that: Bytes) = bytes > that.bytes
    def <= (that: Bytes) = bytes <= that.bytes
    def >= (that: Bytes) = bytes >= that.bytes
  }

  implicit def bytes2int(b: Bytes): Int = b.toInt

  implicit class CyborgInt2BytesMethods(val i: Int) extends AnyVal {
    def bytes = Bytes(i)
    def KiB = Bytes(i * Bytes.KiB)
    def MiB = Bytes(i * Bytes.MiB)
    def GiB = Bytes(i * Bytes.GiB)
    def TiB = Bytes(i * Bytes.TiB)
  }

  implicit class BooleanCyborgExt(val value: Boolean) extends AnyVal {
    def toByte = if (value) 1.toByte else 0.toByte
  }

  implicit class ByteCyborgExt(val byte: Byte) extends AnyVal {
    def hexString: String = "%02X" format byte
    def toBoolean: Boolean = byte != 0.toByte
  }

  implicit class ByteArrayCyborgBinaryExt(val data: Array[Byte]) extends AnyVal {
    def toHexString(separator: String) = data.map("%02x" format _).mkString(separator)
    def toHexString: String = toHexString("")
    def sameBytesAs (that: Array[Byte]): Boolean = {
      if (data.size == that.size) {
        // ugly for speed
        var i = 0
        while (i < data.size) {
          if (data(i) != that(i)) return false
          i += 1
        }
        true
      }
      else false
    }
    def base64: String = Base64.encodeToString(data, Base64.DEFAULT)
    def sha1: Array[Byte] = MessageDigest.getInstance("SHA-1").digest(data)
    def toShort: Short = ByteBuffer.wrap(data).getShort
  }

  implicit class StringCyborgBinaryExt(val string: String) extends AnyVal {
    def decodeBase64: Array[Byte] = Base64.decode(string, Base64.DEFAULT)
    def utf8: Array[Byte] = string.getBytes("UTF8")
  }

  def arrayByteBuffer(size: Int) = ByteBuffer.wrap(Array.ofDim[Byte](size))
  implicit class ByteBufferCyborgExt(val data: ByteBuffer) extends AnyVal {
    def hexString = data.array().map("%02X" format _).mkString(" ")
    def pad(byte: Byte): ByteBuffer = data.put(Array.fill[Byte](data.remaining())(byte))

    def << (byte: Byte): ByteBuffer = data.put(byte)
    def << (int: Int): ByteBuffer = data.putInt(int)
    def << (short: Short): ByteBuffer = data.putShort(short)
    def << (bytes: Array[Byte]): ByteBuffer = data.put(bytes)
    def << (string: String): ByteBuffer = data.put(string.getBytes("UTF-8"))

    def getByteArray(n: Int): Array[Byte] = {
      val dst = Array.ofDim[Byte](n)
      data.get(dst, 0, n)
      dst
    }
  }

  case class InvalidHexString(message: String) extends Exception(message)
  private val spacesRegex = """\s+""".r
  private val hexCharsRegex = """[0-9A-F]+""".r
  implicit class HexStringCyborgExt(val string: String) extends AnyVal {
    def hexToByteArray(minSize: Int = 0): Array[Byte] = {
      val preprocess = spacesRegex replaceAllIn (string, "") toUpperCase()
      if (preprocess.isEmpty)
        Array.fill(minSize)(0.toByte)
      else {
        if (hexCharsRegex unapplySeq preprocess isEmpty)
          throw InvalidHexString(s"'$preprocess' is not a hex string")
        (for (byte <- preprocess.grouped(2)) yield
          Integer.parseInt(byte, 16).toByte).toArray.reverse.padTo(minSize, 0.toByte).reverse
      }
    }

    def hexToByteArrayOpt(minSize: Int = 0): Option[Array[Byte]] =
      catching(classOf[InvalidHexString]).opt(hexToByteArray(minSize))

  }
}
