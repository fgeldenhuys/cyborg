package cyborg.widget

import android.view.{LayoutInflater, ViewGroup, View}
import android.widget.{ListView, TextView, BaseAdapter}
import cyborg.{LogBase, Activity, Context}
import cyborg.view.implicits._
import cyborg.widget.Accessors._
import scala.collection.mutable
import android.os.SystemClock
import cyborg.Context._

class LogAdapter(val textViewResource: Int,
                 val listView: ListView,
                 val activity: Activity,
                 val bufferSize: Int = 64,
                 val logToConsole: Boolean = true)
                (implicit val context: Context) extends BaseAdapter with LogBase {
  val logs = mutable.ListBuffer.empty[String]

  def getCount: Int = logs.size

  def getItem(i: Int): AnyRef = logs(i)

  def getItemId(i: Int): Long = i

  def getView(i: Int, convert: View, parent: ViewGroup): View = {
    val view =
      if (convert != null) convert.asInstanceOf[TextView]
      else {
        val inflate = context.systemService[LayoutInflater]
        inflate[TextView](textViewResource, parent)
      }
    view.text = logs(i)
    view
  }

  def apply(message: String) {
    activity.runOnUiThread {
      logs += message
      if (logs.length > bufferSize) {
        logs.trimStart(1)
      }
      notifyDataSetChanged()
      listView.setSelection(getCount - 1)
    }
  }

  def $d(message: => String) {
    if (logToConsole) cyborg.Log.$d(message)
    apply(message)
  }

  def $i(message: => String) {
    if (logToConsole) cyborg.Log.$i(message)
    apply(message)
  }

  def $w(message: => String) {
    if (logToConsole) cyborg.Log.$w(message)
    apply("WARNING " + message)
  }

  def $e(message: => String) {
    if (logToConsole) cyborg.Log.$e(message)
    apply("ERROR " + message)
  }
}
