package cyborg.widget

import android.widget.RelativeLayout.{LayoutParams => RLLP}
import cyborg.view.{LayoutParams, ViewGroupTrait}
import LayoutParams._
import cyborg.Context
import cyborg.Units.Ltrb

class RelativeLayout(implicit context: Context)
  extends android.widget.RelativeLayout(context) with ViewGroupTrait {

  implicit def layout = this
  implicit val layoutParams = new RLLP(Fill, Fill)
  implicit def defaultLayoutParams = DefaultLayoutParams[RLLP](new RLLP(Wrap, Wrap))
}
