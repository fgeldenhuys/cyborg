import java.io.OutputStream

import scalaz._, Scalaz._, scalaz.concurrent._

import cyborg.util.io._

object io {
  implicit object PdfDocumentToOutputStream extends WriteToOutputStream[android.graphics.pdf.PdfDocument] {
    override def write(a: android.graphics.pdf.PdfDocument, out: OutputStream, progress: (Int) => Unit): Task[Int] = Task.delay {
      a.writeTo(out)
      0
    }
  }
}
