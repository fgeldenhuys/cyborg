package cyborg.view

import android.view.{View, ViewGroup, LayoutInflater}

object implicits {
  case class ViewNotFoundException(id: Int) extends Exception(s"View with id=$id not found")

  implicit class LayoutInflaterCyborgExt(val layoutInflater: LayoutInflater) extends AnyVal {
    def apply[T](resource: Int, root: ViewGroup, attach: Boolean = false): T =
      layoutInflater.inflate(resource, root, attach).asInstanceOf[T]
  }

  implicit class ViewCyborgExt(val view: View) {
    def findById[T](id: Int): T = {
      val result = view.findViewById(id).asInstanceOf[T]
      if (result == null) throw ViewNotFoundException(id)
      result
    }
  }
}
