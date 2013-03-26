package cyborg.view

import android.view.ViewGroup.{LayoutParams => VGLP}
import android.widget.{RelativeLayout => RL}
import android.widget.LinearLayout.{LayoutParams => LLLP}
import android.widget.RelativeLayout.{LayoutParams => RLLP}
import cyborg.Units.Ltrb

// TODO: continue with this plan to create implicit linked list of history of layoutparams
/*
trait LayoutParams {
  def current: VGLP
  def previous: Option[LayoutParams]
  def find[LP <: VGLP]: Option[LP] = {
    if (current.isInstanceOf[LP]) Some(current.asInstanceOf[LP])
    else previous.map(_.find[LP]).getOrElse(None)
  }
}
class ViewGroupLayoutParams(val current: VGLP)(implicit val previous: Option[LayoutParams] = None)
  extends LayoutParams
class LinearLayoutParams(current: LLLP)(implicit previous: Option[LayoutParams] = None)
  extends ViewGroupLayoutParams(current)(previous)
class RelativeLayoutParams(current: RLLP)(implicit previous: Option[LayoutParams] = None)
  extends ViewGroupLayoutParams(current)(previous)
*/

object LayoutParams {
  case class DefaultLayoutParams[LP <: VGLP](layoutParams: LP)

  val Fill = VGLP.FILL_PARENT
  val Wrap = VGLP.WRAP_CONTENT

  // General
  def width[LP <: VGLP](pixels: Int)(implicit layoutParams: LP) { layoutParams.width = pixels }
  def height[LP <: VGLP](pixels: Int)(implicit layoutParams: LP) { layoutParams.height = pixels }
  def size[LP <: VGLP](width: Int, height: Int)(implicit layoutParams: LP) {
    layoutParams.width = width
    layoutParams.height = height
  }

  // Linear Layout
  def gravity(value: Int)(implicit layoutParams: LLLP) { layoutParams.gravity = value }
  def weight(value: Float)(implicit layoutParams: LLLP) { layoutParams.weight = value }
  def weight(value: Double)(implicit layoutParams: LLLP) { layoutParams.weight = value.toFloat }
  def linearMargins(value: Ltrb)(implicit layoutParams: LLLP) { layoutParams.setMargins(value.left, value.top, value.right, value.bottom) }

  // Relative Layout
  def relativeMargins(value: Ltrb)(implicit layoutParams: RLLP) { layoutParams.setMargins(value.left, value.top, value.right, value.bottom) }

  // Relative to parent
  def center(implicit layoutParams: RLLP) { layoutParams.addRule(RL.CENTER_IN_PARENT) }
  def centerHorizontal(implicit layoutParams: RLLP) { layoutParams.addRule(RL.CENTER_HORIZONTAL) }
  def centerVertical(implicit layoutParams: RLLP) { layoutParams.addRule(RL.CENTER_VERTICAL) }
  def alignTop(implicit layoutParams: RLLP) { layoutParams.addRule(RL.ALIGN_PARENT_TOP) }
  def alignBottom(implicit layoutParams: RLLP) { layoutParams.addRule(RL.ALIGN_PARENT_BOTTOM) }
  def alignLeft(implicit layoutParams: RLLP) { layoutParams.addRule(RL.ALIGN_PARENT_LEFT) }
  def alignRight(implicit layoutParams: RLLP) { layoutParams.addRule(RL.ALIGN_PARENT_RIGHT) }

  // Relative to other
  def above(that: ViewTrait)(implicit layoutParams: RLLP) { layoutParams.addRule(RL.ABOVE, that.id) }
  def below(that: ViewTrait)(implicit layoutParams: RLLP) { layoutParams.addRule(RL.BELOW, that.id) }
  def leftOf(that: ViewTrait)(implicit layoutParams: RLLP) { layoutParams.addRule(RL.LEFT_OF, that.id) }
  def rightOf(that: ViewTrait)(implicit layoutParams: RLLP) { layoutParams.addRule(RL.RIGHT_OF, that.id) }
  def alignTopWith(that: ViewTrait)(implicit layoutParams: RLLP) { layoutParams.addRule(RL.ALIGN_TOP, that.id) }
  def alignBottomWith(that: ViewTrait)(implicit layoutParams: RLLP) { layoutParams.addRule(RL.ALIGN_BOTTOM, that.id) }
  def alignLeftWith(that: ViewTrait)(implicit layoutParams: RLLP) { layoutParams.addRule(RL.ALIGN_LEFT, that.id) }
  def alignRightWith(that: ViewTrait)(implicit layoutParams: RLLP) { layoutParams.addRule(RL.ALIGN_RIGHT, that.id) }



}
