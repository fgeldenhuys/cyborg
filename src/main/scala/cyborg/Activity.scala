package cyborg

import android.app.AlertDialog
import android.content.res.Configuration
import android.content.{BroadcastReceiver, DialogInterface}
import android.view.inputmethod.InputMethodManager
import android.view.View
import android.widget.{EditText, Toast}
import Context._
import cyborg.util.events.Observable
import scala.concurrent._
import scala.collection.mutable

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

  sealed trait ToastDuration { def value: Int }
  val ToastLong = new ToastDuration { def value = Toast.LENGTH_LONG }
  val ToastShort = new ToastDuration { def value: Int = Toast.LENGTH_SHORT }

  def toast(message: String, duration: ToastDuration = ToastShort)(implicit activity: android.app.Activity): Toast = {
    val t = Toast.makeText(activity, message, duration.value)
    activity.runOnUiThread(new Runnable { def run() { t.show() } })
    t
  }

  def alert(title: String, message: String)(implicit context: Context): Future[Boolean] = {
    val p = promise[Boolean]
    try {
      val dialog = new AlertDialog.Builder(context)
      dialog.setTitle(title).setMessage(message)
      dialog.setPositiveButton("Ok", new DialogInterface.OnClickListener {
        def onClick(dialog: DialogInterface, button: Int) { p success true }
      })
      dialog.show()
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        p failure e
    }
    p.future
  }

  def prompt(title: String, message: String)(implicit context: Context): Future[Option[String]] = {
    val p = promise[Option[String]]
    try {
      val dialog = new AlertDialog.Builder(context)
      dialog.setTitle(title).setMessage(message)
      val input = new EditText(context)
      dialog.setView(input)
      dialog.setPositiveButton("Ok", new DialogInterface.OnClickListener {
        def onClick(dialog: DialogInterface, button: Int) { p success Some(input.getText.toString) }
      })
      dialog.setNegativeButton("Cancel", new DialogInterface.OnClickListener {
        def onClick(dialog: DialogInterface, button: Int) { p success None }
      })
      dialog.show()
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        p failure e
    }
    p.future
  }

  implicit class ActivityObservableExtensions[T](val obs: Observable[T, Any]) extends AnyVal {
    def subOnUiThread(f: (T) => Any)(implicit activity: Activity): (T) => Any = {
      obs sub { v => activity.runOnUiThread(f(v)) }
    }
  }
}
