package cyborg.droid.context

import android.app.{AlarmManager, DownloadManager, ActivityManager}
import android.view.{LayoutInflater, View}
import android.net.ConnectivityManager
import android.hardware.usb.UsbManager
import android.content.Context
import android.print.PrintManager
import android.view.inputmethod.InputMethodManager

object CyborgContext {
  trait SystemServiceGetter[T] { def get(implicit context: Context): T }
  class SimpleSystemServiceGetter[T](name: String) extends SystemServiceGetter[T] {
    def get(implicit context: Context): T = context.getSystemService(name).asInstanceOf[T]
  }

  implicit val activityManagerGetter = new SimpleSystemServiceGetter[ActivityManager](android.content.Context.ACTIVITY_SERVICE)
  implicit val alarmManagerGetter = new SimpleSystemServiceGetter[AlarmManager](android.content.Context.ALARM_SERVICE)
  implicit val connectivityManagerGetter = new SimpleSystemServiceGetter[ConnectivityManager](android.content.Context.CONNECTIVITY_SERVICE)
  implicit val downloadManagerGetter = new SimpleSystemServiceGetter[DownloadManager](android.content.Context.DOWNLOAD_SERVICE)
  implicit val layoutInflaterGetter = new SimpleSystemServiceGetter[LayoutInflater](android.content.Context.LAYOUT_INFLATER_SERVICE)
  implicit val printManagerGetter = new SimpleSystemServiceGetter[PrintManager](android.content.Context.PRINT_SERVICE)
  implicit val usbManagerGetter = new SimpleSystemServiceGetter[UsbManager](android.content.Context.USB_SERVICE)
  implicit val inputMethodManagerGetter = new SimpleSystemServiceGetter[InputMethodManager](android.content.Context.INPUT_METHOD_SERVICE)

  implicit class CyborgContextExt(val context: android.content.Context) extends AnyVal {
    def systemService[T](implicit getter: SystemServiceGetter[T]): T = getter.get(context)
  }
}
