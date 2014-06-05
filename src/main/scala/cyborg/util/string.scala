package cyborg.util

import java.util.zip.Adler32

object string {

  implicit class CyborgStringExt(val s: String) extends AnyVal {
    def /+/ (t: String): String = {
      if (s.endsWith("/") && t.startsWith("/")) s + t.drop(1)
      else if (s.endsWith("/")) s + t
      else if (t.startsWith("/")) s + t
      else s + "/" + t
    }

    def adler32: Long = {
      val checksum = new Adler32
      val bytes = s.getBytes("UTF-8")
      checksum.update(bytes)
      checksum.getValue
    }
  }

}
