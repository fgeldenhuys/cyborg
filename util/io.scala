package cyborg.util

import java.io._
import binary.Bytes
import android.hardware.usb.{UsbEndpoint, UsbDeviceConnection}
import scala.concurrent.duration.Duration

object io {
  val BufferSize = 1024
  val MaxBufferSize = 16 * 1024 * 1024

  case class FileTooLargeException(message: String) extends IOException(message)

  def inStream2outStream(in: InputStream, out: OutputStream): Int = {
    val buf = new Array[Byte](BufferSize)
    var total = 0
    var bytesRead = in.read(buf, 0, BufferSize)
    while(bytesRead != -1) {
      total += bytesRead
      out.write(buf, 0, bytesRead)
      bytesRead = in.read(buf, 0, BufferSize)
    }
    out.flush()
    total
  }

  def inStream2outStreamWithProgress(in: InputStream, out: OutputStream)(f: (Bytes) => Any): Int = {
    val buf = new Array[Byte](BufferSize)
    var total = 0
    var bytesRead = in.read(buf, 0, BufferSize)
    while(bytesRead != -1) {
      total += bytesRead
      out.write(buf, 0, bytesRead)
      f(Bytes(total))
      bytesRead = in.read(buf, 0, BufferSize)
    }
    out.flush()
    f(Bytes(total))
    total
  }

  def inStream2NewFile(in: InputStream, file: File): Int = {
    val out = new BufferedOutputStream(new FileOutputStream(file, false))
    inStream2outStream(in, out)
  }

  implicit class OutputStreamExt(val out: OutputStream) extends AnyVal {
    def << (in: InputStream) { inStream2outStream(in, out) }
    def << (string: String) { out.write(string.getBytes("UTF-8")) }
  }

  implicit class FileExt(val file: File) extends AnyVal {
    def read: Array[Byte] = {
      val fileSize = file.length().toInt
      if (fileSize > MaxBufferSize)
        throw FileTooLargeException(s"File is $fileSize bytes large, maximum buffer is $MaxBufferSize bytes")
      val in = new BufferedInputStream(new FileInputStream(file))
      val buffer = Array.ofDim[Byte](fileSize)
      in.read(buffer)
      in.close()
      buffer
    }
  }

  implicit class UsbDeviceConnectionExt(val udc: UsbDeviceConnection) extends AnyVal {
    def bulkRead(in: UsbEndpoint, bytes: Int, timeout: Duration): Option[Array[Byte]] = {
      val buffer = new Array[Byte](bytes)
      val read = udc.bulkTransfer(in, buffer, bytes, timeout.toMillis.toInt)
      if (read >= 0) Some(buffer) else None
    }

    def bulkWrite(out: UsbEndpoint, data: Array[Byte], timeout: Duration): Boolean = {
      import cyborg.Log._
      //$d(s"writing ${data.size} bytes")
      val write = udc.bulkTransfer(out, data, data.size, timeout.toMillis.toInt)
      //if (write != data.size) $w(s"write $write != data.size ${data.size}")
      write == data.size
    }
  }

  case class Usb(connection: UsbDeviceConnection, in: UsbEndpoint, out: UsbEndpoint) {
    def bulkRead(bytes: Int, timeout: Duration): Option[Array[Byte]] =
      connection.bulkRead(in, bytes, timeout)
    def bulkWrite(data: Array[Byte], timeout: Duration): Boolean =
      connection.bulkWrite(out, data, timeout)
  }
}
