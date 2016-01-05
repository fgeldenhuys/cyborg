package cyborg.graphics

import android.content.Context
import android.util.TypedValue

object Pixels {
  implicit class IntPixelConversions(val value: Int) extends AnyVal {
    def dp(implicit context: Context) =
      TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, value.toFloat, context.getResources.getDisplayMetrics)

    def sp(implicit context: Context) =
      TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_SP, value.toFloat, context.getResources.getDisplayMetrics)
  }
}
