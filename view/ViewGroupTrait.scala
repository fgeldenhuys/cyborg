package cyborg.view

trait ViewGroupTrait extends android.view.ViewGroup with ViewTrait {
  def += (view: ViewTrait): ViewTrait = {
    addView(view)
    view
  }

}
