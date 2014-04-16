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
    def height = b.getHeight
    def width = b.getWidth

    def compressedBytes(format: BitmapFormat.Format): Option[Array[Byte]] = {
      val out = new ByteArrayOutputStream()
      val success = b.compress(format.format, format.quality, out)
      if (success) Some(out.toByteArray)
      else None
    }

    def scaled(w: Int, h: Int): B =
      B.createScaledBitmap(b, w, h, false)
    def scaleHeight(h: Int): B = {
      val w = ((h.toFloat / b.getHeight.toFloat) * b.getWidth.toFloat).round
      scaled(w, h)
    }

    def cropped(x: Int, y: Int, w: Int, h: Int): B = {
      val px = math.max(x, 0)
      val py = math.max(y, 0)
      val pw = math.max(w, 0)
      val ph = math.max(h, 0)
      val bw = b.getWidth
      val bh = b.getHeight
      B.createBitmap(b, x, y,
        if (px + pw > bw) bw - px else pw,
        if (py + ph > bh) bh - py else ph)
    }
  }

  def makeBitmap(bytes: Array[Byte]): Option[B] = tryOption(BF.decodeByteArray(bytes, 0, bytes.length))
  def makeBitmap(file: File): Option[B] = tryOption(BF.decodeFile(file.getAbsolutePath))
  def makeBitmap(in: InputStream): Option[B] = tryOption(BF.decodeStream(in))
}
