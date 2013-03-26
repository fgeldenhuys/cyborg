package cyborg.view

import cyborg.widget._

trait Style[T <: ViewTrait] {
  def apply(view: T)
}

object DefaultStyles {

  implicit val buttonDefaultStyle = new Style[ButtonTrait] {
    def apply(view: ButtonTrait) {
      view.setTextColor(android.graphics.Color.BLACK)
    }
  }

  implicit val gridViewDefaultStyle = new Style[GridViewTrait] {
    def apply(view: GridViewTrait) {
    }
  }

  implicit val listViewDefaultStyle = new Style[ListViewTrait] {
    def apply(view: ListViewTrait) {
    }
  }

  implicit val textViewDefaultStyle = new Style[TextViewTrait] {
    def apply(view: TextViewTrait) {
      view.setTextColor(android.graphics.Color.BLACK)
    }
  }

}