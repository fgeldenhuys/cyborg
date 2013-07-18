package cyborg.widget

import android.widget.{FrameLayout => FL}
import android.widget.FrameLayout.{LayoutParams => FLLP}
import cyborg.Context
import cyborg.view.ViewGroupTrait
import cyborg.view.LayoutParams._

class FrameLayout(implicit context: Context) extends FL(context) with ViewGroupTrait {
  implicit def layout = this
  implicit val layoutParams = new FLLP(Fill, Fill)
  implicit def defaultLayoutParams = DefaultLayoutParams[FLLP](new FLLP(Wrap, Wrap))
}
