package cyborg.widget

import android.widget.{LinearLayout => LL}
import android.widget.LinearLayout.{LayoutParams => LLLP}
import cyborg.view.{LayoutParams, ViewGroupTrait}
import LayoutParams._
import cyborg.Context

class VerticalLayout(implicit context: Context) extends LL(context) with ViewGroupTrait {
  implicit def layout = this
  implicit val layoutParams = new LLLP(Fill, Fill)
  implicit def defaultLayoutParams = DefaultLayoutParams[LLLP](new LLLP(Wrap, Wrap))

  setOrientation(LL.VERTICAL)
}
