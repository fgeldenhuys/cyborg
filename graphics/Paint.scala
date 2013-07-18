package cyborg.graphics

object Paint {
  type Style = android.graphics.Paint.Style

  val Fill = android.graphics.Paint.Style.FILL
  val Stroke = android.graphics.Paint.Style.STROKE
  val FillAndStroke = android.graphics.Paint.Style.FILL_AND_STROKE

  lazy val SolidBlack = Paint(color = Color.Black)
  lazy val LineBlack = Paint(color = Color.Black, style = Stroke, antiAlias = true)
  lazy val SolidWhite = Paint(color = Color.White)

  def apply(
        antiAlias: Boolean = false,
        color: Int = Color.Black,
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


}
