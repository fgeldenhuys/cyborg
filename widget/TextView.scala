package cyborg.widget

import cyborg.Activity
import cyborg.view._
import cyborg.view.LayoutParams.DefaultLayoutParams
import android.view.ViewGroup.{LayoutParams => VGLP}
import android.view.View.OnClickListener
import android.text.method.TransformationMethod
import cyborg.Units.Ltrb
import android.text.{Editable, TextWatcher}

trait TextViewTrait extends android.widget.TextView with ViewTrait {
  protected val defaultTextWatcher = new TextWatcher {
    var onTextChangedFun: Option[() => Unit] = None
    def beforeTextChanged(s: CharSequence, start: Int, count: Int, after: Int) {}
    def onTextChanged(s: CharSequence, start: Int, before: Int, count: Int) { onTextChangedFun.map(_()) }
    def afterTextChanged(s: Editable) {}
  }

  def colour: Int = getCurrentTextColor
  def colour_=(colour: Int) { setTextColor(colour) }
  def hint = getHint
  def hint_=(h: CharSequence) { setHint(h) }
  def text: CharSequence = getText
  def text_=(text: CharSequence) { setText(text) }
  def textSize = getTextSize
  def textSize_=(s: Double) { setTextSize(s.toFloat) }

  def transformationMethod: TransformationMethod = getTransformationMethod
  def transformationMethod_=(tm: TransformationMethod) { setTransformationMethod(tm) }
  def imeOptions = getImeOptions
  def imeOptions_=(o: Int) { setImeOptions(o) }
  def inputType = getInputType
  def inputType_=(it: Int) { setInputType(it) }

  def lineSpacing(add: Double, mult: Double) { setLineSpacing(add.toFloat, mult.toFloat) }

  def onClick(f: => Unit) { setOnClickListener(new OnClickListener { def onClick(v: android.view.View) { f } }) }
  def onTextChanged(f: => Unit) { defaultTextWatcher.onTextChangedFun = Some(() => f) }
}

class TextView[LP <: VGLP](implicit activity: Activity,
                           layout: ViewGroupTrait,
                           defaultLayoutParams: DefaultLayoutParams[LP],
                           style: Style[TextViewTrait])
  extends android.widget.TextView(activity) with TextViewTrait {
  implicit val layoutParams = defaultLayoutParams.layoutParams

  id = activity.nextUniqueId
  setLayoutParams(layoutParams)
  style(this)
  layout += this
}
