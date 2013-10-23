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

  def toast(message: String)(implicit activity: android.app.Activity) {
    activity.runOnUiThread(new Runnable {
      def run() { Toast.makeText(activity, message, Toast.LENGTH_LONG).show() }
    })
  }

  def prompt(title: String, message: String)(implicit context: Context): Future[Option[String]] = {
    val p = promise[Option[String]]()
    val alert = new AlertDialog.Builder(context)
    alert.setTitle(title)
    alert.setMessage(message)
    val input = new EditText(context)
    alert.setView(input)
    alert.setPositiveButton("Ok", new DialogInterface.OnClickListener {
      def onClick(dialog: DialogInterface, button: Int) { p success Some(input.getText.toString) }
    })
    alert.setNegativeButton("Cancel", new DialogInterface.OnClickListener {
      def onClick(dialog: DialogInterface, button: Int) { p success None }
    })
    alert.show()
    p.future
  }

  implicit class ActivityObservableExtensions[T](val obs: Observable[T, Any]) extends AnyVal {
    def subOnUiThread(f: (T) => Any)(implicit activity: Activity): (T) => Any = {
      obs sub { v => activity.runOnUiThread(f(v)) }
    }
  }
}
