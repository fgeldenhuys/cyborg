package cyborg

import Context._
import android.view.View
import android.view.inputmethod.InputMethodManager
import android.widget.Toast
import android.content.res.Configuration
import cyborg.util.events.Observable

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

  def isLandscape =
    getResources.getConfiguration.orientation == Configuration.ORIENTATION_LANDSCAPE

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

  def toast(message: String)(implicit activity: android.app.Activity) {
    activity.runOnUiThread(new Runnable {
      def run() { Toast.makeText(activity, message, Toast.LENGTH_LONG).show() }
    })
  }

  implicit class ActivityObservableExtensions[T](val obs: Observable[T, Any]) extends AnyVal {
    def subOnUiThread(f: (T) => Any)(implicit activity: Activity): (T) => Any = {
      obs sub { v => activity.runOnUiThread(f(v)) }
    }
  }
}
