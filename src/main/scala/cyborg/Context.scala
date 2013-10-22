package cyborg

import android.app.{DownloadManager, ActivityManager}
import android.view.LayoutInflater
import android.hardware.usb.UsbManager

object Context {
  implicit def android2cyborgContext(context: android.content.Context) = new Context(context)
  implicit def cyborg2androidContext(context: Context) = context.c

  trait SystemServiceGetter[T] { def get(implicit context: Context): T }
  class SimpleSystemServiceGetter[T](name: String) extends SystemServiceGetter[T] {
    def get(implicit context: Context): T = context.getSystemService(name).asInstanceOf[T]
  }
  implicit val activityManagerGetter = new SimpleSystemServiceGetter[ActivityManager](android.content.Context.ACTIVITY_SERVICE)
  implicit val downloadManagerGetter = new SimpleSystemServiceGetter[DownloadManager](android.content.Context.DOWNLOAD_SERVICE)
  implicit val layoutInflaterGetter = new SimpleSystemServiceGetter[LayoutInflater](android.content.Context.LAYOUT_INFLATER_SERVICE)
  implicit val usbManagerGetter = new SimpleSystemServiceGetter[UsbManager](android.content.Context.USB_SERVICE)

  class Context(val c: android.content.Context) extends AnyVal {
    def systemService[T](implicit getter: SystemServiceGetter[T]): T = getter.get(c)
  }
}
