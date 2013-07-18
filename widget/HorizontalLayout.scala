package cyborg.widget

import android.widget.{LinearLayout => LL}
import android.widget.LinearLayout.{LayoutParams => LLLP}
import cyborg.Context
import cyborg.view.ViewGroupTrait
import cyborg.view.LayoutParams._
import android.view.ViewGroup.{LayoutParams => VGLP}

class HorizontalLayout[LP <: VGLP](parentLayoutParams: LP = new LLLP(Fill, Fill))
                                  (implicit context: Context) extends LL(context) with ViewGroupTrait {
  implicit def layout = this
  implicit val layoutParams = parentLayoutParams
  implicit def defaultLayoutParams = DefaultLayoutParams[LLLP](new LLLP(Wrap, Wrap))

  setOrientation(LL.HORIZONTAL)

  def weightSum = getWeightSum.toDouble
  def weightSum_=(ws: Double) { setWeightSum(ws.toFloat) }
}
