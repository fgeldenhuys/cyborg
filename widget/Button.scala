package cyborg.widget

import android.view.ViewGroup.{LayoutParams => VGLP}
import cyborg.Activity
import cyborg.view._

import LayoutParams.DefaultLayoutParams

trait ButtonTrait extends android.widget.Button with TextViewTrait {

}

class Button[LP <: VGLP](implicit activity: Activity,
                         layout: ViewGroupTrait,
                         defaultLayoutParams: DefaultLayoutParams[LP],
                         style: Style[ButtonTrait])
  extends android.widget.Button(activity) with ButtonTrait {
  implicit val layoutParams = defaultLayoutParams.layoutParams

  id = activity.nextUniqueId
  setLayoutParams(layoutParams)
  style(this)
  layout += this
}
