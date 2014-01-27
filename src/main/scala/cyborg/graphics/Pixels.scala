package cyborg.graphics

import cyborg.Context.Context
import android.util.TypedValue

object Pixels {
  implicit class IntPixelConversions(val value: Int) extends AnyVal {
    def dp(implicit context: Context) =
      TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, value.toFloat, context.resources.getDisplayMetrics)
      //(value * context.resources.getDisplayMetrics.density + 0.5f).toInt

    def sp(implicit context: Context) =
      TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_SP, value.toFloat, context.resources.getDisplayMetrics)
  }
}
