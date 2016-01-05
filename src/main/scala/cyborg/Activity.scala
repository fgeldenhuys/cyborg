package cyborg

import android.app.AlertDialog
import android.content.DialogInterface
import android.content.DialogInterface.OnClickListener
import android.content.res.Configuration
import android.graphics.Rect
import android.view.inputmethod.InputMethodManager
import android.view.View
import android.widget.{EditText, Toast}
import Context._
import cyborg.Log._
import cyborg.util.execution.ScheduledExecutionContext
import cyborg.util.task._
import java.util.concurrent.atomic.AtomicInteger
import scalaz.concurrent.{Promise => PromiseZ}
import scalaz.concurrent.Task
import scalaz._

@deprecated("Use CyborgActivity", ":)")
class Activity extends android.app.Activity {
  implicit val context: Context = this
  implicit val activity: Activity = this

  private val idSequence = new java.util.concurrent.atomic.AtomicInteger(0)
  def nextUniqueId = {
    var candidate: Int = idSequence.incrementAndGet
    while(findViewById(candidate) != null) candidate = idSequence.incrementAndGet
    candidate
  }

  @deprecated("Use the function on the object")
  def runOnUiThreadOld(f: => Any)(implicit sec: ScheduledExecutionContext): Unit = {
    try {
      super.runOnUiThread(new Runnable {
        def run(): Unit = {
          cyborg.util.debug.warnAfterTime(1000)(f)
        }
      })
    } catch {
      case t: Throwable =>
        $w("runOnUiThread FAILED : " + t)
        t.printStackTrace()
    }
  }

  def findView[T <: View](id: Int): T = findViewById(id).asInstanceOf[T]

  def invalidateRootView(): Unit = {
    findViewById(android.R.id.content).invalidate()
  }

  def isLandscape =
    getResources.getConfiguration.orientation == Configuration.ORIENTATION_LANDSCAPE

  def screenSize: (Int, Int) = {
    val display = getWindowManager.getDefaultDisplay
    (display.getWidth, display.getHeight)
  }

  def displaySize: (Int, Int) = {
    val rect = new Rect()
    getWindow.getDecorView.getWindowVisibleDisplayFrame(rect)
    (rect.width(), rect.height())
  }

}

object Activity {
  sealed trait ActivityResultCode { def code: Int }
  case class ActivityResultOk(code: Int = android.app.Activity.RESULT_OK) extends ActivityResultCode
  case class ActivityResultCanceled(code: Int = android.app.Activity.RESULT_CANCELED) extends ActivityResultCode

  sealed trait DialogResult
  case object DialogPositiveResult extends DialogResult
  case object DialogNeutralResult extends DialogResult
  case object DialogNegativeResult extends DialogResult

  val ResultOk = android.app.Activity.RESULT_OK
  val ResultCanceled = android.app.Activity.RESULT_CANCELED

  def makeResultCode(code: Int): Option[ActivityResultCode] = {
    code match {
      case android.app.Activity.RESULT_OK => Some(ActivityResultOk())
      case android.app.Activity.RESULT_CANCELED => Some(ActivityResultCanceled())
      case _ => None
    }
  }

  sealed trait ToastDuration { def value: Int }
  val ToastLong = new ToastDuration { def value = Toast.LENGTH_LONG }
  val ToastShort = new ToastDuration { def value: Int = Toast.LENGTH_SHORT }

  case class ActivityResult(code: ActivityResultCode, data: Option[android.content.Intent])

  class IntentCallbacks {
    val map = scala.collection.mutable.HashMap.empty[Int, PromiseZ[ActivityResult]]
    var nextRequestCode = new AtomicInteger(1001)

    def onActivityResult(requestCode: Int, result: ActivityResult): Boolean = {
      map.get(requestCode).fold (false) { p =>
        p fulfill result
        map remove requestCode
        true
      }
    }
  }

  def runOnUiThread(f: => Any)(implicit activity: Activity, sec: ScheduledExecutionContext): Unit = {
    val st = cyborg.util.debug.getStackTrace
    try {
      activity.runOnUiThread(new Runnable {
        def run(): Unit = {
          cyborg.util.debug.warnAfterTimeWith(1000, st)(f)
        }
      })
    } catch {
      case t: Throwable =>
        $w("runOnUiThread FAILED : " + t)
        t.printStackTrace()
    }
  }

  def inUiThread = android.os.Looper.myLooper() == android.os.Looper.getMainLooper()

  def startIntentForResult(intent: android.content.Intent)
                          (implicit activity: android.app.Activity, ic: IntentCallbacks): PromiseZ[ActivityResult] = {
    import scalaz.concurrent.Promise._
    val rc = ic.nextRequestCode.addAndGet(1)
    val p = emptyPromise[ActivityResult]
    ic.map += ((rc, p))
    activity.startActivityForResult(intent, rc)
    p
  }

  def startIntentSenderForResult(is: android.content.IntentSender)
    (implicit activity: android.app.Activity, ic: IntentCallbacks): PromiseZ[ActivityResult] = {
    import scalaz.concurrent.Promise._
    val rc = ic.nextRequestCode.addAndGet(1)
    val p = emptyPromise[ActivityResult]
    ic.map += ((rc, p))
    activity.startIntentSenderForResult(is, rc, null, 0, 0, 0)
    p
  }

  def toast(message: String, duration: ToastDuration = ToastShort)(implicit activity: android.app.Activity): Toast = {
    val t = Toast.makeText(activity, message, duration.value)
    activity.runOnUiThread(new Runnable { def run(): Unit = { t.show() } })
    t
  }

  def alert(title: String, message: String)(implicit activity: android.app.Activity): scala.concurrent.Future[Boolean] = {
    import scala.concurrent._
    val p = promise[Boolean]
    try {
      val dialog = new AlertDialog.Builder(activity)
      dialog.setTitle(title).setMessage(message)
      dialog.setPositiveButton("Ok", new DialogInterface.OnClickListener {
        def onClick(dialog: DialogInterface, button: Int): Unit = { \/.fromTryCatch(p success true) }
      })
      activity.runOnUiThread(new Runnable {
        override def run(): Unit = {
          \/.fromTryCatch(dialog.show()).leftMap(_.printStackTrace())
        }
      })
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        \/.fromTryCatch(p failure e)
    }
    p.future
  }

  def confirm(title: String, message: String, positive: String = "Yes", negative: String = "No")
             (implicit activity: android.app.Activity): PromiseZ[DialogResult] = {
    import scalaz.concurrent.Promise._
    val p = emptyPromise[DialogResult]
    try {
      val dialog = new AlertDialog.Builder(activity)
      dialog.setTitle(title).setMessage(message)
      dialog.setPositiveButton(positive, new DialogInterface.OnClickListener {
        def onClick(dialog: DialogInterface, button: Int): Unit = { p.fulfill(DialogPositiveResult) }
      })
      dialog.setNegativeButton(negative, new DialogInterface.OnClickListener {
        def onClick(dialog: DialogInterface, button: Int): Unit = { p.fulfill(DialogNegativeResult) }
      })
      activity.runOnUiThread(new Runnable {
        override def run(): Unit = { dialog.show() }
      })
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        p.break
    }
    p
  }

  def confirmWithNeutral(title: String, message: String, positive: String, neutral: String, negative: String)
                        (implicit activity: android.app.Activity): PromiseZ[DialogResult] = {
    import scalaz.concurrent.Promise._
    val p = emptyPromise[DialogResult]
    try {
      val dialog = new AlertDialog.Builder(activity)
      dialog.setTitle(title).setMessage(message)
      dialog.setPositiveButton(positive, new DialogInterface.OnClickListener {
        def onClick(dialog: DialogInterface, button: Int): Unit = { p.fulfill(DialogPositiveResult) }
      })
      dialog.setNeutralButton(neutral, new OnClickListener {
        override def onClick(p1: DialogInterface, p2: Int): Unit = { p.fulfill(DialogNeutralResult) }
      })
      dialog.setNegativeButton(negative, new DialogInterface.OnClickListener {
        def onClick(dialog: DialogInterface, button: Int): Unit = { p.fulfill(DialogNegativeResult) }
      })
      activity.runOnUiThread(new Runnable {
        override def run(): Unit = { dialog.show() }
      })
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        p.break
    }
    p
  }

  def prompt(title: String, message: String, text: String = "")
            (implicit activity: android.app.Activity): scala.concurrent.Future[Option[String]] = {
    import scala.concurrent._
    val p = promise[Option[String]]
    try {
      val dialog = new AlertDialog.Builder(activity)
      dialog.setTitle(title).setMessage(message)
      val input = new EditText(activity)
      input.setText(text)
      dialog.setView(input)
      dialog.setPositiveButton("Ok", new DialogInterface.OnClickListener {
        def onClick(dialog: DialogInterface, button: Int): Unit = { p success Some(input.getText.toString) }
      })
      dialog.setNegativeButton("Cancel", new DialogInterface.OnClickListener {
        def onClick(dialog: DialogInterface, button: Int): Unit = { p success None }
      })
      activity.runOnUiThread(new Runnable {
        override def run(): Unit = { dialog.show() }
      })
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        p failure e
    }
    p.future
  }

  def hideKeyboard(activity: android.app.Activity): Unit = {
    val inputManager = activity
      .getSystemService(android.content.Context.INPUT_METHOD_SERVICE)
      .asInstanceOf[InputMethodManager]
    inputManager
      .hideSoftInputFromWindow(activity.getCurrentFocus.getWindowToken,
      InputMethodManager.HIDE_NOT_ALWAYS)
  }

}
