package cyborg

import cyborg.Context._

object Units {
  class DisplayMetric(val value: Double)(implicit context: Context) {
    def dip: Int = (value * context.getResources.getDisplayMetrics.density).toInt
    def sp: Int = (value * context.getResources.getDisplayMetrics.scaledDensity).toInt
    def pctW: Int = ((value / 100.0) * context.getResources.getDisplayMetrics.widthPixels.toDouble).toInt
    def pctH: Int = ((value / 100.0) * context.getResources.getDisplayMetrics.heightPixels.toDouble).toInt
  }
  implicit def double2displayMetric(value: Double)(implicit context: Context) = new DisplayMetric(value)(context)
  implicit def int2displayMetric(value: Int)(implicit context: Context) = new DisplayMetric(value)(context)

  case class Ltrb(left: Int, top: Int, right: Int, bottom: Int)
  object Ltrb {
    def same(x: Int) = Ltrb(x, x, x, x)
    def symm(lr: Int, tb: Int) = Ltrb(lr, tb, lr, tb)
    def none = Ltrb(0, 0, 0, 0)
  }
  implicit def tuple2ltrb(t: (Int, Int, Int, Int)) = Ltrb(t._1, t._2, t._3, t._4)

  /* doesn't work because layout.getWidth is still 0
  class PercentageMetric(val pct: Double)(implicit layout: ViewGroupTrait) {
    def pctW: Int = ((pct / 100.0) * layout.getWidth.toDouble).toInt
    def pctH: Int = ((pct / 100.0) * layout.getHeight.toDouble).toInt
  }
  implicit def double2percentageMetric(pct: Double)(implicit layout: ViewGroupTrait) = new PercentageMetric(pct)(layout)
  implicit def int2percentageMetric(pct: Int)(implicit layout: ViewGroupTrait) = new PercentageMetric(pct)(layout)
  */
}
