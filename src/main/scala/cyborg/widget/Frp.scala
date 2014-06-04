package cyborg.widget

import io.dylemma.frp._

object Frp {
  def clicks(v: android.view.View): EventStream[Unit] = {
    import android.view.View, View.OnClickListener
    val es = EventSource[Unit]
    v.setOnClickListener(new OnClickListener {
      override def onClick(v: View) { es fire Unit }
    })
    es
  }
}
