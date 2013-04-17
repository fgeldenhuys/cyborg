package cyborg

object util {
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

    def printStackTrace() {
      $d((new Throwable()).getStackTrace/*.drop(4)*/.mkString("\n>  "))
    }
  }
}
