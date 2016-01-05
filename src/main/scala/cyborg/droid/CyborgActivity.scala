package cyborg.droid

import android.app.Activity
import android.app.AlertDialog
import android.content.DialogInterface
import android.widget.EditText

import scalaz._, Scalaz._, scalaz.concurrent._

import cyborg.droid.context.CyborgContext._
import cyborg.util.task._

object CyborgActivity {
  implicit class CyborgActivityExt(val activity: Activity) extends AnyVal {

    def alert(title: String, message: String)
             (implicit uiThread: UiThread): Promise[Boolean] = {
      val promise = Promise.emptyPromise[Boolean]
      \/.fromTryCatchNonFatal {
        val dialog = new AlertDialog.Builder(activity)
        dialog.setTitle(title).setMessage(message)
        dialog.setPositiveButton("Ok", new DialogInterface.OnClickListener {
          def onClick(dialog: DialogInterface, button: Int): Unit = promise.fulfill(true)
        })
        uiThread(dialog.show())
      } .leftMap { t =>
        t.printStackTrace()
        promise.break
      }
      promise
    }

    def prompt(title: String, message: String, text: String = ""): Task[Option[String]] =
      Task.async { register =>
        \/.fromTryCatchNonFatal {
          val dialog = new AlertDialog.Builder(activity)
          dialog.setTitle(title).setMessage(message)
          val input = new EditText(activity)
          input.setText(text)
          dialog.setView(input)
          dialog.setPositiveButton("Ok", new DialogInterface.OnClickListener {
            def onClick(dialog: DialogInterface, button: Int): Unit =
              register(\/-(Some(input.getText.toString)))
          })
          dialog.setNegativeButton("Cancel", new DialogInterface.OnClickListener {
            def onClick(dialog: DialogInterface, button: Int): Unit =
              register(\/-(None))
          })
          activity.runOnUiThread(new Runnable {
            override def run(): Unit = { dialog.show() }
          })
        } .leftMap (t => register(-\/(t)))
      }

    def hideKeyboard(): Unit = \/.fromTryCatchNonFatal {
      import android.view.inputmethod.InputMethodManager
      activity.systemService[InputMethodManager]
        .hideSoftInputFromWindow(activity.getCurrentFocus.getWindowToken,
        InputMethodManager.HIDE_NOT_ALWAYS)
    }

  }

  sealed trait ActivityResultCode { def code: Int }
  case object ActivityResultOk extends ActivityResultCode {
    override val code: Int = android.app.Activity.RESULT_OK
  }
  case object ActivityResultCanceled extends ActivityResultCode {
    override val code: Int = android.app.Activity.RESULT_CANCELED
  }
  case class ActivityResultUnknown(code: Int) extends ActivityResultCode
  object ActivityResultCode {
    def apply(code: Int): ActivityResultCode = {
      code match {
        case android.app.Activity.RESULT_OK => ActivityResultOk
        case android.app.Activity.RESULT_CANCELED => ActivityResultCanceled
        case other => ActivityResultUnknown(other)
      }
    }
  }

}
