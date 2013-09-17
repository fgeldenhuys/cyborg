package cyborg

import android.content.Intent
import android.os.IBinder
import cyborg.Context._

class Service extends android.app.Service {
  implicit val context: Context = this

  def onBind(intent: Intent): IBinder = null
}

object Service {
  val StartSticky = android.app.Service.START_STICKY
  val StartNotSticky = android.app.Service.START_NOT_STICKY
}
