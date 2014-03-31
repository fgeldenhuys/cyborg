package cyborg.graphics

import android.graphics.{Bitmap => B, BitmapFactory => BF}
import java.io.{InputStream, File, ByteArrayOutputStream}
import cyborg.util.control._

object Bitmap {
  object BitmapFormat {
    sealed trait Format {
      def format: B.CompressFormat
      def quality: Int
    }
    case class Jpeg(quality: Int = 80) extends Format { def format = B.CompressFormat.JPEG }
    case class Png(quality: Int = 100) extends Format { def format = B.CompressFormat.PNG }
    //WARNING: WebP transparency support was only added in Android 4.2
    case class WebP(quality: Int = 100) extends Format { def format = B.CompressFormat.WEBP }
  }

  implicit class CyborgBitmapExt(val b: B) extends AnyVal {
    def compressedBytes(format: BitmapFormat.Format): Option[Array[Byte]] = {
      val out = new ByteArrayOutputStream()
      val success = b.compress(format.format, format.quality, out)
      if (success) Some(out.toByteArray)
      else None
    }
  }

  def makeBitmap(bytes: Array[Byte]): Option[B] = tryOption(BF.decodeByteArray(bytes, 0, bytes.length))
  def makeBitmap(file: File): Option[B] = tryOption(BF.decodeFile(file.getAbsolutePath))
  def makeBitmap(in: InputStream): Option[B] = tryOption(BF.decodeStream(in))
}
