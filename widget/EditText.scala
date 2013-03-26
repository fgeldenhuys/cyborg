package cyborg.widget

import android.view.ViewGroup.{LayoutParams => VGLP}
import cyborg.Activity
import cyborg.view.{Style, ViewGroupTrait}
import cyborg.view.LayoutParams.DefaultLayoutParams

trait EditTextTrait extends android.widget.EditText with TextViewTrait {

}

class EditText[LP <: VGLP](implicit activity: Activity,
                           layout: ViewGroupTrait,
                           defaultLayoutParams: DefaultLayoutParams[LP],
                           style: Style[TextViewTrait])
  extends android.widget.EditText(activity) with EditTextTrait {
  implicit val layoutParams = defaultLayoutParams.layoutParams

  id = activity.nextUniqueId
  setLayoutParams(layoutParams)
  style(this)
  layout += this
}
