package cyborg

import android.content.Intent
import android.os.IBinder

class Service extends android.app.Service {
  def onBind(intent: Intent): IBinder = null
}

object Service {
  val StartSticky = android.app.Service.START_STICKY
  val StartNotSticky = android.app.Service.START_NOT_STICKY
}
