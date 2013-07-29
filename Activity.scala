package cyborg

import Context._
import android.view.View
import android.view.inputmethod.InputMethodManager

class Activity extends android.app.Activity {
  implicit val context: Context = this
  implicit val activity: Activity = this

  private val idSequence = new java.util.concurrent.atomic.AtomicInteger(0)
  def nextUniqueId = {
    var candidate: Int = idSequence.incrementAndGet
    while(findViewById(candidate) != null) candidate = idSequence.incrementAndGet
    candidate
  }

  def runOnUiThread(f: => Any) {
    super.runOnUiThread(new Runnable {
      def run() { f }
    })
  }

  def findView[T <: View](id: Int): T = findViewById(id).asInstanceOf[T]

  def hideKeyboard() {
    val inputManager = getSystemService(android.content.Context.INPUT_METHOD_SERVICE)
      .asInstanceOf[InputMethodManager]
    inputManager.hideSoftInputFromWindow(getCurrentFocus.getWindowToken,
      InputMethodManager.HIDE_NOT_ALWAYS)
  }
}

object Activity {
  val ResultOk = android.app.Activity.RESULT_OK
  val ResultCanceled = android.app.Activity.RESULT_CANCELED
}
