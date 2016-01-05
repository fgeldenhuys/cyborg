package cyborg.graphics

object Paint {
  type Style = android.graphics.Paint.Style

  val AntiAliasFlag = android.graphics.Paint.ANTI_ALIAS_FLAG
  val Fill = android.graphics.Paint.Style.FILL
  val FillAndStroke = android.graphics.Paint.Style.FILL_AND_STROKE
  val Stroke = android.graphics.Paint.Style.STROKE

  lazy val SolidBlack = Paint(color = Color.Black)
  lazy val LineBlack = Paint(color = Color.Black, style = Stroke, antiAlias = true)
  lazy val SolidWhite = Paint(color = Color.White)

  def apply(
        antiAlias: Boolean = false,
        color: Color = Color.Black,
        strokeWidth: Float = 0f,
        style: Style = Fill
      ): android.graphics.Paint = {
    val p = new android.graphics.Paint()
    p.setAntiAlias(antiAlias)
    p.setColor(color)
    p.setStrokeWidth(strokeWidth)
    p.setStyle(style)
    p
  }

  implicit class PaintExt(val paint: android.graphics.Paint) extends AnyVal {
    def alpha(a: Int) = { paint.setAlpha(a); paint }
    def color(c: Color) = { paint.setColor(c.value); paint }
    def dither(b: Boolean) = { paint.setDither(b); paint }
    def shader(s: android.graphics.Shader) = { paint.setShader(s); paint }
    def strokeWidth(w: Float) = { paint.setStrokeWidth(w); paint }
    def style(s: android.graphics.Paint.Style) = { paint.setStyle(s); paint }
  }

  implicit class TextPaintExt(val paint: android.text.TextPaint) extends AnyVal {
    def antiAlias(b: Boolean) = { paint.setAntiAlias(b); paint }
    def color(c: Color) = { paint.setColor(c.value); paint }
    def fakeBold(b: Boolean) = { paint.setFakeBoldText(b); paint }
    def shadowLayer(r: Float, dx: Float, dy: Float, c: Int) = { paint.setShadowLayer(r, dx, dy, c); paint }
    def strokeWidth(w: Float) = { paint.setStrokeWidth(w); paint }
    def style(s: android.graphics.Paint.Style) = { paint.setStyle(s); paint }
    def textAlign(a: android.graphics.Paint.Align) = { paint.setTextAlign(a); paint }
    def textSize(s: Float) = { paint.setTextSize(s); paint }
    //def typeface(t: Int) = { paint.setTypeface(android.graphics.Typeface.defaultFromStyle(t)); paint }
  }

}
