package cyborg

object Context {
  implicit def android2cyborgContext(context: android.content.Context) = new Context(context)
  implicit def cyborg2androidContext(context: Context) = context.c
  val UsbService = android.content.Context.USB_SERVICE
  val DownloadService = android.content.Context.DOWNLOAD_SERVICE
  val LayoutInflaterService = android.content.Context.LAYOUT_INFLATER_SERVICE
}

class Context(val c: android.content.Context) extends AnyVal {
  def systemService[A](name: String): A = c.getSystemService(name).asInstanceOf[A]
}
