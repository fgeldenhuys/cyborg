package cyborg.util

import java.io._
import java.nio.channels.Channels
import java.nio.ByteBuffer

import scala.concurrent._
import scala.concurrent.duration.Duration
import scalaz.concurrent.Task

//import android.hardware.usb.{UsbEndpoint, UsbDeviceConnection}

import cyborg.exceptions.JsonParseError
import cyborg.util.binary._
import cyborg.util.control._
import cyborg.util.execution.ScheduledExecutionContext
import cyborg.util.string.Path
import cyborg.util.task._
import cyborg.util.scalazext._
import scalaz._
import argonaut._


object io {
  val BufferSize = 1024 * 32
  val MaxBufferSize = 1024 * 1024


  // Input Streams

  implicit class InputStreamOps(val in: InputStream) extends AnyVal {
    def drop(n: Int) = {
      \/.fromTryCatchNonFatal(in.skip(n.toLong))
      in
    }

    // This will read the entire stream, discard the data, and return bytes read
    // The stream should definitely already be buffered
    def countBytes: Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        var count = 0l
        while (in.read() != -1) count += 1
        in.close()
        count
      }
    }
  }


  trait InputStreamMaker[A] {
    def makeInputStream(a: A): Throwable \/ InputStream
  }

  def inputStream[A](a: A)(implicit M: InputStreamMaker[A]) = M.makeInputStream(a)

  def inputStreamByteArrayUpdateReader(in: InputStream)(update: (Array[Byte], Int) => Unit): Throwable \/ Int = {
    \/.fromTryCatchNonFatal {
      val buf = new Array[Byte](BufferSize)
      var n = 0
      var i = in.read(buf, 0, BufferSize)
      while (i != -1) {
        update(buf, i)
        n += i
        i = in.read(buf, 0, BufferSize)
      }
      n
    }
  }

  implicit object StringInputStreamMaker extends InputStreamMaker[String] {
    override def makeInputStream(a: String): Throwable \/ InputStream =
      \/.fromTryCatchNonFatal(new ByteArrayInputStream(a.getBytes("UTF-8")))
  }

  implicit object FileInputStreamMaker extends InputStreamMaker[File] {
    override def makeInputStream(a: File): Throwable \/ InputStream =
      \/.fromTryCatchNonFatal(new BufferedInputStream(new FileInputStream(a)))
  }

  implicit object ByteArrayInputStreamMaker extends InputStreamMaker[Array[Byte]] {
    override def makeInputStream(a: Array[Byte]): Throwable \/ InputStream =
      \/.fromTryCatchNonFatal(new ByteArrayInputStream(a))
  }


  trait InputStreamConverter[A] {
    def readAs(in: InputStream): Throwable \/ A
  }

  implicit class InputStreamConverterMethods(val in: InputStream) extends AnyVal {
    def readAs[A](implicit C: InputStreamConverter[A]) = C.readAs(in)
  }

  implicit class InputStreamThrowableConverterMethods(val either: Throwable \/ InputStream) extends AnyVal {
    def readAs[A](implicit C: InputStreamConverter[A]) = either.flatMap(C.readAs)
  }

  implicit object ByteArrayInputStreamConverter extends InputStreamConverter[Array[Byte]] {
    override def readAs(in: InputStream): Throwable \/ Array[Byte] = \/.fromTryCatchNonFatal {
      val buffer = new ByteArrayOutputStream()
      inStream2outStream(in, buffer)
      in.close()
      buffer.toByteArray
    }
  }

  implicit object StringInputStreamConverter extends InputStreamConverter[String] {
    override def readAs(in: InputStream): Throwable \/ String =
      ByteArrayInputStreamConverter.readAs(in).flatMap(x => \/.fromTryCatchNonFatal(new String(x, "UTF-8")))
  }

  implicit object JsonInputStreamConverter extends InputStreamConverter[Json] {
    override def readAs(in: InputStream): Throwable \/ Json =
      StringInputStreamConverter.readAs(in).flatMap(s => Parse.parse(s)
        .leftMap(e => JsonParseError(e, Some(s))))
  }








  // Output Streams
  trait WriteToOutputStream[A] {
    def write(a: A, out: OutputStream, progress: Int => Unit): Task[Int]
  }

  implicit class WriteToOutputStreamOps[A](val a: A) extends AnyRef {
    def writeToOutputStreamProgress(out: OutputStream)(progress: Int => Unit)
                                   (implicit W: WriteToOutputStream[A]): Task[Int] =
      W.write(a, out, progress)

    def writeToOutputStream(out: OutputStream)(implicit W: WriteToOutputStream[A]): Task[Int] =
      writeToOutputStreamProgress(out)(_ => Unit)

    def writeToFileProgress(file: File)(progress: Int => Unit)(implicit W: WriteToOutputStream[A]): Task[Int] =
      Task.delay(new FileOutputStream(file)).flatMap(out => W.write(a, out, progress))

    def writeToFile(file: File)(implicit W: WriteToOutputStream[A]): Task[Int] =
      writeToFileProgress(file)(_ => Unit)

    def appendToFileProgress(file: File)(progress: Int => Unit)(implicit W: WriteToOutputStream[A]): Task[Int] =
      Task.delay(new FileOutputStream(file, true)).flatMap(out => W.write(a, out, progress))

    def appendToFile(file: File)(implicit W: WriteToOutputStream[A]): Task[Int] =
      appendToFileProgress(file)(_ => Unit)
  }

  implicit object ByteArrayWriteToOutputStream extends WriteToOutputStream[Array[Byte]] {
    override def write(a: Array[Byte], out: OutputStream, progress: Int => Unit): Task[Int] = Task.delay {
      for ((p, n) <- (0 to (a.length-1) by BufferSize).map(x => (x, math.min(BufferSize, a.length - x)))) {
        out.write(a, p, n)
        progress(p + n)
      }
      out.close()
      a.length
    }
  }

  implicit object StringWriteToOutputStream extends WriteToOutputStream[String] {
    override def write(a: String, out: OutputStream, progress: Int => Unit): Task[Int] =
      ByteArrayWriteToOutputStream.write(a.getBytes("UTF-8"), out, progress)
  }

  implicit object StringEitherWriteToOutputStream extends WriteToOutputStream[Throwable \/ String] {
    override def write(a: Throwable \/ String, out: OutputStream, progress: Int => Unit): Task[Int] =
      StringWriteToOutputStream.write(a.getOrThrow, out, progress)
  }

  implicit object InputStreamToOutputStream extends WriteToOutputStream[InputStream] {
    override def write(a: InputStream, out: OutputStream, progress: (Int) => Unit): Task[Int] = Task.delay {
      val src = Channels.newChannel(a)
      val dst = Channels.newChannel(out)
      val buffer = ByteBuffer.allocateDirect(BufferSize)
      var n = 0
      while (src.read(buffer) != -1) {
        buffer.flip()
        n += dst.write(buffer)
        buffer.compact()
        progress(n)
      }
      buffer.flip()
      while (buffer.hasRemaining) dst.write(buffer)
      progress(n)
      n
    }
  }

  implicit object InputStreamThrowableToOutputStream extends WriteToOutputStream[Throwable \/ InputStream] {
    override def write(a: Throwable \/ InputStream, out: OutputStream, progress: (Int) => Unit): Task[Int] = {
      import cyborg.util.task._
      Task.delayThrowableV(a).flatMap(InputStreamToOutputStream.write(_, out, progress))
    }
  }







  // Files

  trait FileOpener[A] {
    def openFile(a: A): Throwable \/ File
  }

  def openFile[A](a: A)(implicit F: FileOpener[A]) = F.openFile(a)
  def openFileTask[A](a: A)(implicit F: FileOpener[A]) = Task.delayThrowableV(F.openFile(a))
  def fileExists[A](a: A)(implicit F: FileOpener[A]) = F.openFile(a).exists(_.exists())
  def fileSize[A](a: A)(implicit F: FileOpener[A]) = F.openFile(a).map(_.length)

  implicit object FileOpenerIdentity extends FileOpener[File] {
    override def openFile(a: File): Throwable \/ File = \/-(a)
  }

  implicit object PathFileOpener extends FileOpener[Path] {
    override def openFile(a: Path): Throwable \/ File = \/.fromTryCatchNonFatal(new File(a.toString))
  }

  implicit object StringFileOpener extends FileOpener[String] {
    override def openFile(a: String): Throwable \/ File = \/.fromTryCatchNonFatal(new File(a))
  }














  case class FileTooLargeException(message: String) extends IOException(message)

  private lazy val doubleDotRegex = "[^/]+/\\.\\.".r
  private lazy val singleDotRegex = "/\\./".r
  private lazy val multiSlashRegex = "/+".r
  def makeCanonicalPath(current: String, cd: String): String = {
    var result = if (cd.startsWith("/")) cd // Absolute path
                 else if (!current.contains("/")) cd // Current is file
                 else current.replaceFirst("/[^/]*$", "/") + cd // Relative path
    while (doubleDotRegex.findFirstIn(result).isDefined) result = doubleDotRegex.replaceAllIn(result, "")
    result = singleDotRegex.replaceAllIn(result, "/")
    multiSlashRegex.replaceAllIn(result, "/")
  }

  def openExistingFile(path: String) = openFile(Path(path)).toOption.filter(_.exists())
  def openTempFile(prefix: String, suffix: String, directory: File = null) =
    tryEither(File.createTempFile(prefix, suffix, directory))

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

  def inStream2outStreamM(in: InputStream, out: OutputStream)
                         (implicit sec: ScheduledExecutionContext): Monitor[Int, Bytes] = {
    val m = monitor[Int, Bytes]
    future {
      val buf = new Array[Byte](BufferSize)
      var total = 0
      var bytesRead = in.read(buf, 0, BufferSize)
      while(bytesRead != -1 && !m.isCompleted) {
        total += bytesRead
        out.write(buf, 0, bytesRead)
        m.reportProgress(Bytes(total))
        bytesRead = in.read(buf, 0, BufferSize)
      }
      out.flush()
      if (!m.isCompleted) m success total
    }
    m
  }

  def inStream2NewFile(in: InputStream, file: File): Int = {
    val out = new BufferedOutputStream(new FileOutputStream(file, false))
    inStream2outStream(in, out)
  }

  def inStream2NewFileM(in: InputStream, file: File)
                       (implicit sec: ScheduledExecutionContext): Monitor[Int, Bytes] = {
    val out = new BufferedOutputStream(new FileOutputStream(file, false))
    inStream2outStreamM(in, out)
  }

  implicit class OutputStreamCyborgExt(val out: OutputStream) extends AnyVal {
    def << (in: InputStream): OutputStream = { inStream2outStream(in, out); out }
    def << (string: String): OutputStream = { out.write(string.getBytes("UTF-8")); out }
    def << (byte: Byte): OutputStream = { out.write(byte.toInt); out }
    def << (int: Int): OutputStream = { out.write(arrayByteBuffer(4) << int array()); out }
    def << (bytes: Array[Byte]): OutputStream = { out.write(bytes); out }
    def encString (string: String): OutputStream = {
      val bytes = string.getBytes("UTF-8")
      out << bytes.size << bytes
      out
    }
  }

  implicit class InputStreamCyborgExt(val in: InputStream) extends AnyVal {
    def bytes(size: Int): Array[Byte] = {
      val buffer = Array.ofDim[Byte](size)
      in.read(buffer)
      buffer
    }
    def bytes(): Array[Byte] = {
      val out = new ByteArrayOutputStream()
      inStream2outStream(in, out)
      out.toByteArray
    }
    def byte: Byte = bytes(1)(0)
    def int: Int = ByteBuffer.wrap(bytes(4)).getInt
    def string(size: Int): String = new String(bytes(size), "UTF-8") //WARNING: size in bytes, not encoded characters
    def string: String = {
      val out = new ByteArrayOutputStream()
      inStream2outStream(in, out)
      out.toString("UTF-8")
    }
    def decString: String = {
      val size = in.int
      val bytes = in.bytes(size)
      new String(bytes, "UTF-8")
    }
  }

  implicit class FileCyborgExt(val file: File) extends AnyVal {
    def name = file.getName

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

    def readBytes(bytes: Int): Array[Byte] = {
      val in = new BufferedInputStream(new FileInputStream(file))
      val buffer = Array.ofDim[Byte](bytes)
      in.read(buffer)
      in.close()
      buffer
    }

    def readString: String = new String(read, "UTF-8")

    def write(data: Array[Byte]): Throwable \/ File = \/.fromTryCatchNonFatal {
      val out = new BufferedOutputStream(new FileOutputStream(file))
      out.write(data)
      out.close()
      file
    }

    def writeString(data: String): Throwable \/ File = write(data.getBytes("UTF-8"))

    def validZipFile: Boolean = {
      try {
        new java.util.zip.ZipFile(file)
        true
      }
      catch {
        case e: Throwable => false
      }
    }

    def unzipStream(path: String): Throwable \/ InputStream = \/.fromTryCatchNonFatal {
      val zip = new java.util.zip.ZipFile(file)
      zip.getInputStream(zip.getEntry(path))
    }

    def sha1: Array[Byte] = read.sha1
    def sha1Sample(bytes: Int): Array[Byte] = readBytes(bytes).sha1

    def ls: Throwable \/ List[File] = \/.fromTryCatchNonFatal(file.listFiles().toList)
  }

  implicit class ByteArrayCyborgIOExt(val data: Array[Byte]) extends AnyVal {
    def writeToFile(file: String): Unit = {
      val out = new FileOutputStream(file)
      out.write(data)
      out.close()
    }
  }

  object ZipExtractors {
    object ZipFileEntry {
      def unapply(entry: java.util.zip.ZipEntry): Option[(String, Long, Long)] = entry.isDirectory match {
        case false => Some((entry.getName, entry.getSize, entry.getTime))
        case true => None
      }
    }
    object ZipDirectoryEntry {
      def unapply(entry: java.util.zip.ZipEntry): Option[(String, Long, Long)] = entry.isDirectory match {
        case true => Some((entry.getName, entry.getSize, entry.getTime))
        case false => None
      }
    }
  }

  /*
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
  */

}
