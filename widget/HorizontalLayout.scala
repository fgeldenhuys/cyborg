package cyborg.widget

import android.widget.{LinearLayout => LL}
import android.widget.LinearLayout.{LayoutParams => LLLP}
import cyborg.Context._
import cyborg.view.ViewGroupTrait
import cyborg.view.LayoutParams._

class HorizontalLayout(implicit context: Context) extends LL(context) with ViewGroupTrait {
  implicit def layout = this
  implicit val layoutParams = new LLLP(Fill, Fill)
  implicit def defaultLayoutParams = DefaultLayoutParams[LLLP](new LLLP(Wrap, Wrap))

  setOrientation(LL.HORIZONTAL)

  def weightSum = getWeightSum.toDouble
  def weightSum_=(ws: Double) { setWeightSum(ws.toFloat) }
}
