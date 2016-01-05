package cyborg

import android.content.Context
import cyborg.droid.context._, CyborgContext._
import android.view.{LayoutInflater, ViewGroup}
import android.graphics.{BitmapFactory, Bitmap}

object resources {
  // [A <: android.view.View] would be nice... :/
  class ViewResource[A](val id: Int) {
    def find(implicit activity: android.app.Activity): A = {
      assert(activity != null)
      activity.findViewById(id).asInstanceOf[A]
    }

    def inflate(root: ViewGroup, attach: Boolean = false)
               (implicit context: Context): A =
      cyborg.Context.cyborg2androidContext(context).systemService[LayoutInflater].inflate(id, root, attach).asInstanceOf[A]
  }

  trait Resource[T] {
    def id: Int
    def apply(implicit context: Context): T
  }

  case class BitmapResource(id: Int) extends Resource[Bitmap] {
    def apply(implicit context: Context): Bitmap = BitmapFactory.decodeResource(context.getResources, id)
  }

  case class StringResource(id: Int) extends Resource[String] {
    def apply(implicit context: Context): String = context.getResources.getString(id)
  }

  trait ResourceGetter[T] {
    def apply(id: Int): Resource[T]
  }

  implicit val bitmapResourceGetter = new ResourceGetter[Bitmap] {
    override def apply(id: Int): Resource[Bitmap] = BitmapResource(id)

  }

  implicit val stringResourceGetter = new ResourceGetter[String] {
    override def apply(id: Int): Resource[String] = StringResource(id)

  }

  def resource[T](id: Int)(implicit resourceGetter: ResourceGetter[T]): Resource[T] = resourceGetter(id)
}
