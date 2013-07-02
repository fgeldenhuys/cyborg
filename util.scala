package cyborg

import android.os.{Handler, Debug}
import cyborg.Context.Context

object util {
  case class NotImplemented(message: String = "Not Implemented") extends Exception(message)
  case class InvalidConversion(message: String = "Invalid Conversion") extends Exception(message)

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

  object numbers {
    object ValidInt {
      def unapply(i: BigInt): Option[Int] = if (i.isValidInt) Some(i.toInt) else None
    }
    object ValidLong {
      def unapply(l: BigInt): Option[Long] = if (l.isValidLong) Some(l.toLong) else None
    }
  }

  object io {
    import java.io._

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

    def inStream2NewFile(in: InputStream, file: File): Int = {
      val out = new BufferedOutputStream(new FileOutputStream(file, false))
      inStream2outStream(in, out)
    }

    implicit class OutputStreamExt(val out: OutputStream) extends AnyVal {
      def << (in: InputStream) { inStream2outStream(in, out) }
      def << (string: String) { out.write(string.getBytes("UTF-8")) }
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

  object debug {
    import cyborg.Log._

    /*implicit class Tapper[A](what: A) {
      def tap(f: (A) => Any): A = {
        f(what)
        what
      }
    }*/

    def printStackTrace() {
      $d((new Throwable()).getStackTrace/*.drop(4)*/.mkString("\n>  "))
    }

    def printMemoryUsage() {
      val meminfo = new Debug.MemoryInfo
      Debug.getMemoryInfo(meminfo)
      $d("Memory Usage: Pss=" + meminfo.getTotalPss + " Private=" + meminfo.getTotalPrivateDirty + " Shared=" + meminfo.getTotalSharedDirty())
    }
  }
}
