package cyborg

import cyborg.Context.Context
import android.view.{LayoutInflater, ViewGroup}

object resources {
  // [A <: android.view.View] would be nice... :/
  class ViewResource[A](val id: Int) {
    def find(implicit activity: Activity): A =
      activity.findViewById(id).asInstanceOf[A]

    def inflate(root: ViewGroup, attach: Boolean = false)
               (implicit context: Context): A =
      context.systemService[LayoutInflater].inflate(id, root, attach).asInstanceOf[A]
  }
}
