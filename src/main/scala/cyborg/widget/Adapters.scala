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

  trait ListAdapterMaker[A] extends AdapterMaker[List[A]] {
    def apply(source: List[A]) = new BaseAdapter {
      def getCount: Int = source.size
      def getItem(i: Int): AnyRef = source(i).asInstanceOf[AnyRef]
      def getItemId(id: Int): Long = id
      def getView(i: Int, convert: View, parent: ViewGroup): View =
        makeView(source(i), convert, parent)
    }
    def makeView(item: A, convert: View, parent: ViewGroup): View
  }

  def makeAdapter[A](source: A)(implicit adapterMaker: AdapterMaker[A]): BaseAdapter =
    adapterMaker(source)

  def makeListAdapter[A](f: (A, View, ViewGroup) => View)
                        (implicit context: Context) =
    new ListAdapterMaker[A] {
      def makeView(item: A, convert: View, parent: ViewGroup): View = f(item, convert, parent)
    }

  def makeListAdapterFromShow[A, TV <: TextView](vr: ViewResource[TV])
                                                (implicit s: Show[A], context: Context) =
    new ListAdapterMaker[A] {
      def makeView(item: A, convert: View, parent: ViewGroup): View = {
        val view: TV =
          if (convert != null) convert.asInstanceOf[TV]
          else vr.inflate(parent)(context)
        view.text = item.shows
        view
      }
    }

  def makeListAdapterFromTextView[A, TV <: TextView](vr: ViewResource[TV])
                                                    (f: (A) => String)
                                                    (implicit context: Context) =
    new ListAdapterMaker[A] {
      def makeView(item: A, convert: View, parent: ViewGroup): View = {
        val view: TV =
          if (convert != null) convert.asInstanceOf[TV]
          else vr.inflate(parent)(context)
        view.text = f(item)
        view
      }
    }

  val emptyAdapter = new BaseAdapter {
    def getCount: Int = 0
    def getItem(i: Int): AnyRef = null
    def getItemId(id: Int): Long = id
    def getView(i: Int, convert: View, parent: ViewGroup): View = null
  }
}
