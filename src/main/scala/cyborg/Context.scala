package cyborg

import android.app.{DownloadManager, ActivityManager}
import android.content.{IntentFilter, BroadcastReceiver}
import android.hardware.usb.UsbManager
import android.support.v4.content.LocalBroadcastManager
import android.view.LayoutInflater
import cyborg.Intent._
import cyborg.Log._
import android.net.ConnectivityManager

object Context {
  val ModePrivate = android.content.Context.MODE_PRIVATE
  val ModeWorldReadable = android.content.Context.MODE_WORLD_READABLE
  val ModeWorldWritable = android.content.Context.MODE_WORLD_WRITEABLE
  val ModeMultiProcess = android.content.Context.MODE_MULTI_PROCESS

  implicit def android2cyborgContext(context: android.content.Context) = new Context(context)
  implicit def cyborg2androidContext(context: Context) = context.c

  trait SystemServiceGetter[T] { def get(implicit context: Context): T }
  class SimpleSystemServiceGetter[T](name: String) extends SystemServiceGetter[T] {
    def get(implicit context: Context): T = context.getSystemService(name).asInstanceOf[T]
  }
  implicit val activityManagerGetter = new SimpleSystemServiceGetter[ActivityManager](android.content.Context.ACTIVITY_SERVICE)
  implicit val connectivityManagerGetter = new SimpleSystemServiceGetter[ConnectivityManager](android.content.Context.CONNECTIVITY_SERVICE)
  implicit val downloadManagerGetter = new SimpleSystemServiceGetter[DownloadManager](android.content.Context.DOWNLOAD_SERVICE)
  implicit val layoutInflaterGetter = new SimpleSystemServiceGetter[LayoutInflater](android.content.Context.LAYOUT_INFLATER_SERVICE)
  implicit val usbManagerGetter = new SimpleSystemServiceGetter[UsbManager](android.content.Context.USB_SERVICE)

  class Context(val c: android.content.Context) /* extends AnyVal */ { // nested class not allowed in value class: will be fixed
    assert(c != null)

    def resources = c.getResources
    def systemService[T](implicit getter: SystemServiceGetter[T]): T = getter.get(c)

    def broadcast(action: String, extras: (String, Any)*) {
      val intent = new android.content.Intent(action)
      intent.putExtras(extras:_*)
      //$i(s"BROADCAST '$action' $intent", 1)
      c.sendBroadcast(intent)
    }

    def localBroadcast(action: String, extras: (String, Any)*) {
      val intent = new android.content.Intent(action)
      intent.putExtras(extras:_*)
      //$i(s"BROADCAST local '$action' $intent", 1)
      LocalBroadcastManager.getInstance(c).sendBroadcast(intent)
    }

    def broadcastReceiver(action: String)(f: (android.content.Intent) => Any): CyborgWrappedBroadcastReceiver = {
      val receiver = new BroadcastReceiver {
        def onReceive(context: android.content.Context, intent: android.content.Intent) {
          //$d("RECEIVED " + intent.getAction)
          f(intent)
        }
      }
      new CyborgWrappedBroadcastReceiver(receiver, local=false, c, new IntentFilter(action))
    }

    def localBroadcastReceiver(action: String)(f: (android.content.Intent) => Any): CyborgWrappedBroadcastReceiver = {
      val receiver = new BroadcastReceiver {
        def onReceive(context: android.content.Context, intent: android.content.Intent) {
          //$d("RECEIVED local " + intent.getAction)
          f(intent)
        }
      }
      new CyborgWrappedBroadcastReceiver(receiver, local=true, c, new IntentFilter(action))
    }
  }

  class CyborgWrappedBroadcastReceiver(
    val receiver: BroadcastReceiver,
    val local: Boolean,
    val context: Context,
    val filter: IntentFilter) {

    def register() {
      $d(s"REGISTER receiver for ${filter.getAction(0)}", 1)
      if (local)
        LocalBroadcastManager.getInstance(context).registerReceiver(receiver, filter)
      else
        context.registerReceiver(receiver, filter)
    }

    def unregister() {
      $d(s"UNREGISTER receiver for ${filter.getAction(0)}", 1)
      if (local)
        LocalBroadcastManager.getInstance(context).unregisterReceiver(receiver)
      else
        context.unregisterReceiver(receiver)
    }
  }
  implicit def wrapped2broadcastReceiver(wrapped: CyborgWrappedBroadcastReceiver): BroadcastReceiver = wrapped.receiver

}
