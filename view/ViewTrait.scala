package cyborg.view

import cyborg.Units.Ltrb

trait ViewTrait extends android.view.View {
  /*def selectDynamic(name: String) = {
    val getter = "get" + name.capitalize
    val method = this.getClass.getMethod(getter)
    method.invoke(this)
  }

  def updateDynamic(name: String)(value: Any) = {
    val setter = "set" + name.capitalize
    $d(s"update $name $value $setter ${this.getClass}")
    val method = this.getClass.getMethod(setter, value.getClass)
    $d(s"method $method")
    method.invoke(this, Array[Any](value))
    value
  }*/

  def id: Int = getId
  def id_=(id: Int) { setId(id) }

  def enabled = isEnabled
  def enabled_=(e: Boolean) { setEnabled(e) }

  def padding = Ltrb(getPaddingLeft, getPaddingTop, getPaddingRight, getPaddingBottom)
  def padding_=(p: Ltrb) { setPadding(p.left, p.top, p.right, p.bottom) }

  def backgroundColour = { throw new Exception("not implemented"); 0 }
  def backgroundColour_=(c: Int) { setBackgroundColor(c) }

  def nextFocusForward(view: ViewTrait) { setNextFocusForwardId(view.id) }
  def nextFocusDown(view: ViewTrait) { setNextFocusDownId(view.id) }
  def nextFocusUp(view: ViewTrait) { setNextFocusUpId(view.id) }
  def nextFocusLeft(view: ViewTrait) { setNextFocusLeftId(view.id) }
  def nextFocusRight(view: ViewTrait) { setNextFocusRightId(view.id) }

}
