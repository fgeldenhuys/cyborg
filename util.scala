package cyborg

import android.os.{Handler, Debug}

object util {
  case class NotImplemented(message: String = "Not Implemented") extends Exception(message)
  case class InvalidConversion(message: String = "Invalid Conversion") extends Exception(message)

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
    }

    implicit class ByteArrayExt(val data: Array[Byte]) extends AnyVal {
      def hexString = data.map("%02X" format _).mkString(" ")
    }
  }

  object concurrent {
    import scala.concurrent._
    import scala.concurrent.duration._

    def awaitResultOption[T](awaitable: Awaitable[T], atMost: Duration): Option[T] =
      try {
        Some(Await.result(awaitable, atMost))
      } catch {
        case e: TimeoutException => None
      }

  }

  object control {
    implicit class OptionExt[A](val x: Option[A]) extends AnyVal {
      def failWith(f: => Any): Option[A] = {
        if (x.isDefined) x
        else { f; x }
      }
    }

    def OptionWithFail[A](x: A)(f: => Any): Option[A] = {
      if (x == null) { f; None }
      else Some(x)
    }


  }

  object debug {
    import cyborg.Log._

    /*implicit class Tapper[A](what: A) {
      def tap(f: (A) => Any): A = {
        f(what)
        what
      }
    }*/

    def printStackTrace() {
      $d((new Throwable()).getStackTrace.drop(2).mkString("\n>  "))
    }

    def printMemoryUsage() {
      val meminfo = new Debug.MemoryInfo
      Debug.getMemoryInfo(meminfo)
      $d("Memory Usage: Pss=" + meminfo.getTotalPss + " Private=" + meminfo.getTotalPrivateDirty + " Shared=" + meminfo.getTotalSharedDirty())
    }
  }

  object execution {
    implicit def fun2runnable(f: => Unit): Runnable = new Runnable { def run() { f } }

    def runnable(f: => Unit): Runnable =
      new Runnable {
        def run() { f }
      }

    def runOnMainLooper(f: => Unit)(implicit context: Context) {
      (new Handler(context.getMainLooper)).post(f)
    }
  }

  object io {
    import java.io._
    import binary.Bytes

    val BufferSize = 1024

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
  }

  object numbers {
    object ValidInt {
      def unapply(i: BigInt): Option[Int] = if (i.isValidInt) Some(i.toInt) else None
    }
    object ValidLong {
      def unapply(l: BigInt): Option[Long] = if (l.isValidLong) Some(l.toLong) else None
    }

    object ImplicitDoubleFloatConversions {
      implicit def double2float(d: Double) = d.toFloat
      implicit def float2double(f: Float) = f.toDouble
    }

    object ImplicitDoubleIntConversions {
      implicit def double2int(d: Double) = d.toInt
      implicit def int2double(i: Int) = i.toDouble
    }

    private val WhitespaceRegex = "\\s".r
    implicit class HexStringContext(val sc: StringContext) extends AnyVal {
      def hex(args: Any*): Int = {
        val strings = sc.parts.iterator
        val expressions = args.iterator
        val buffer = new StringBuffer(strings.next)
        while (strings.hasNext) {
          buffer append expressions.next
          buffer append strings.next
        }
        java.lang.Long.parseLong(WhitespaceRegex replaceAllIn (buffer.toString, _ => ""), 16).toInt
      }
    }
  }

}
