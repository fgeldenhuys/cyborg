package cyborg.widget

import android.view.ViewGroup.{LayoutParams => VGLP}
import cyborg.view.{Style, ViewGroupTrait}
import cyborg.Activity
import cyborg.view.LayoutParams.DefaultLayoutParams

trait GridViewTrait extends android.widget.GridView with ViewGroupTrait

class GridView[LP <: VGLP](implicit activity: Activity,
                           val layout: ViewGroupTrait,
                           defaultLayoutParams: DefaultLayoutParams[LP],
                           style: Style[GridViewTrait])
  extends android.widget.GridView(activity) with GridViewTrait {
  implicit val layoutParams = defaultLayoutParams.layoutParams

  id = activity.nextUniqueId
  setLayoutParams(layoutParams)
  style(this)
  layout += this

  def nColumns = getNumColumns
  def nColumns_=(c: Int) { setNumColumns(c) }
  def columnWidth: Int = throw new Exception("not implemented")
  def columnWidth_=(w: Int) { setColumnWidth(w) }

}
