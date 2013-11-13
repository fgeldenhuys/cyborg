package cyborg.widget

import android.view.{ViewGroup, View}
import android.widget.{TextView, BaseAdapter}
import cyborg.Context.Context
import cyborg.resources.ViewResource
import cyborg.widget.Accessors._
import scalaz._, Scalaz._

object Adapters {
  trait AdapterMaker[A] {
    def apply(source: A): BaseAdapter
  }

  def makeAdapter[A](source: A)(implicit adapterMaker: AdapterMaker[A]): BaseAdapter =
    adapterMaker(source)

  def makeListAdapterMapping[A](vr: ViewResource[TextView])
                              (implicit s: Show[A], context: Context) = {
    new AdapterMaker[List[A]] {
      def apply(source: List[A]) = new BaseAdapter {
        def getCount: Int = source.size
        def getItem(i: Int): AnyRef = source(i).asInstanceOf[AnyRef] //yuck
        def getItemId(id: Int): Long = id
        def getView(i: Int, convert: View, parent: ViewGroup): View = {
          val view =
            if (convert != null) convert.asInstanceOf[TextView]
            else vr.inflate(parent)(context)
          view.text = source(i).shows
          view
        }
      }
    }
  }

}
