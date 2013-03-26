package cyborg.widget

import cyborg.view.{Style, ViewGroupTrait}
import android.view.ViewGroup.{LayoutParams => VGLP}
import cyborg.Activity
import cyborg.view.LayoutParams.DefaultLayoutParams

trait ListViewTrait extends android.widget.ListView with ViewGroupTrait

class ListView[LP <: VGLP](implicit activity: Activity,
                           val layout: ViewGroupTrait,
                           defaultLayoutParams: DefaultLayoutParams[LP],
                           style: Style[ListViewTrait])
  extends android.widget.ListView(activity) with ListViewTrait {
  implicit val layoutParams = defaultLayoutParams.layoutParams

  id = activity.nextUniqueId
  setLayoutParams(layoutParams)
  style(this)
  layout += this
}
