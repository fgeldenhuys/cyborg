package cyborg.util

object string {

  implicit class CyborgStringExt(val s: String) extends AnyVal {

    def /+/ (t: String): String = {
      if (s.endsWith("/") && t.startsWith("/")) s + t.drop(1)
      else if (s.endsWith("/")) s + t
      else if (t.startsWith("/")) s + t
      else s + "/" + t
    }
  }

}
