package cyborg

import android.content.{IntentFilter, BroadcastReceiver}
import android.support.v4.content.LocalBroadcastManager
import cyborg.Intent._
import cyborg.Log._

object Context {
  val ModePrivate = android.content.Context.MODE_PRIVATE
  val ModeWorldReadable = android.content.Context.MODE_WORLD_READABLE
  val ModeWorldWritable = android.content.Context.MODE_WORLD_WRITEABLE
  val ModeMultiProcess = android.content.Context.MODE_MULTI_PROCESS

  implicit def android2cyborgContext(context: android.content.Context) = new Context(context)
  implicit def cyborg2androidContext(context: Context) = context.c

  class Context(val c: android.content.Context) /* extends AnyVal */ { // nested class not allowed in value class: will be fixed
    assert(c != null)

    def resources = c.getResources

    def broadcast(action: String, extras: (String, Any)*): Unit = {
      val intent = new android.content.Intent(action)
      intent.putExtras(extras:_*)
      //$i(s"BROADCAST '$action' $intent", 1)
      c.sendBroadcast(intent)
    }

    def localBroadcast(action: String, extras: (String, Any)*): Unit = {
      val intent = new android.content.Intent(action)
      intent.putExtras(extras:_*)
      //$i(s"BROADCAST local '$action' $intent", 1)
      LocalBroadcastManager.getInstance(c).sendBroadcast(intent)
    }

    def broadcastReceiver(action: String)(f: (android.content.Intent) => Any): CyborgWrappedBroadcastReceiver = {
      val receiver = new BroadcastReceiver {
        def onReceive(context: android.content.Context, intent: android.content.Intent): Unit = {
          //$d("RECEIVED " + intent.getAction)
          f(intent)
        }
      }
      new CyborgWrappedBroadcastReceiver(receiver, local=false, c, new IntentFilter(action))
    }

    def localBroadcastReceiver(action: String)(f: (android.content.Intent) => Any): CyborgWrappedBroadcastReceiver = {
      val receiver = new BroadcastReceiver {
        def onReceive(context: android.content.Context, intent: android.content.Intent): Unit = {
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

    def register(): Unit = {
      $d(s"REGISTER receiver for ${filter.getAction(0)}", 1)
      if (local)
        LocalBroadcastManager.getInstance(context).registerReceiver(receiver, filter)
      else
        context.registerReceiver(receiver, filter)
    }

    def unregister(): Unit = {
      $d(s"UNREGISTER receiver for ${filter.getAction(0)}", 1)
      if (local)
        LocalBroadcastManager.getInstance(context).unregisterReceiver(receiver)
      else
        context.unregisterReceiver(receiver)
    }
  }
  implicit def wrapped2broadcastReceiver(wrapped: CyborgWrappedBroadcastReceiver): BroadcastReceiver = wrapped.receiver

  def register(xs: CyborgWrappedBroadcastReceiver*): Unit = { for (x <- xs) x.register() }
  def unregister(xs: CyborgWrappedBroadcastReceiver*): Unit = { for (x <- xs) x.unregister() }
}