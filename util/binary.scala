package cyborg.util

import java.nio.ByteBuffer

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

  implicit class ByteArrayExt(val data: Array[Byte]) extends AnyVal {
    def hexString = data.map("%02X" format _).mkString(" ")
  }

  def arrayByteBuffer(size: Int) = ByteBuffer.wrap(Array.ofDim[Byte](size))
  implicit class ByteBufferExt(val data: ByteBuffer) extends AnyVal {
    def hexString = data.array().map("%02X" format _).mkString(" ")
    def << (byte: Byte): ByteBuffer = data.put(byte)
    def << (int: Int): ByteBuffer = data.putInt(int)
    def << (a: Array[Byte]): ByteBuffer = data.put(a)
    def << (string: String): ByteBuffer = data.put(string.getBytes("UTF-8"))
    def pad(byte: Byte): ByteBuffer = data.put(Array.fill[Byte](data.remaining())(byte))
  }
}
