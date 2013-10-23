package cyborg.util

import java.nio.ByteBuffer
import scala.util.control.Exception._
import java.io.ByteArrayOutputStream
import android.util.Base64

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
  }

  implicit def bytes2int(b: Bytes): Int = b.toInt

  implicit class BooleanCyborgExt(val value: Boolean) extends AnyVal {
    def toByte = if (value) 1.toByte else 0.toByte
  }

  implicit class ByteCyborgExt(val byte: Byte) extends AnyVal {
    def hexString: String = "%02X" format byte
    def toBoolean: Boolean = byte != 0.toByte
  }

  implicit class ByteArrayCyborgExt(val data: Array[Byte]) extends AnyVal {
    def hexString(separator: String) = data.map("%02X" format _).mkString(separator)
    def hexString: String = hexString(" ")
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
    def toShort: Short = ByteBuffer.wrap(data).getShort
  }

  def arrayByteBuffer(size: Int) = ByteBuffer.wrap(Array.ofDim[Byte](size))
  implicit class ByteBufferCyborgExt(val data: ByteBuffer) extends AnyVal {
    def hexString = data.array().map("%02X" format _).mkString(" ")
    def << (byte: Byte): ByteBuffer = data.put(byte)
    def << (int: Int): ByteBuffer = data.putInt(int)
    def << (short: Short): ByteBuffer = data.putShort(short)
    def << (bytes: Array[Byte]): ByteBuffer = data.put(bytes)
    def << (string: String): ByteBuffer = data.put(string.getBytes("UTF-8"))
    def pad(byte: Byte): ByteBuffer = data.put(Array.fill[Byte](data.remaining())(byte))
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
