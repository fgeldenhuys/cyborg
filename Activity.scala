package cyborg

import Context._

class Activity extends android.app.Activity {
  implicit val context: Context = this
  implicit val activity: Activity = this

  private val idSequence = new java.util.concurrent.atomic.AtomicInteger(0)
  def nextUniqueId = {
    var candidate: Int = idSequence.incrementAndGet
    while(findViewById(candidate) != null) candidate = idSequence.incrementAndGet
    candidate
  }

  def runOnUiThread(f: => Unit) {
    runOnUiThread(new Runnable {
      def run() { f }
    })
  }
}

object Activity {
  val ResultOk = android.app.Activity.RESULT_OK
}
