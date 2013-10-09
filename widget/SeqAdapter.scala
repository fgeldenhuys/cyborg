package cyborg.widget

import android.view.{LayoutInflater, ViewGroup, View}
import android.widget.{TextView, BaseAdapter}
import cyborg.Context._
import cyborg.view.implicits._

class SeqAdapter[T](val seq: Seq[T])
                   (val createView: (Int, View, ViewGroup) => View)
  extends BaseAdapter {
  def getCount: Int = seq.length
  def getItem(i: Int): AnyRef = seq(i).asInstanceOf[AnyRef]
  def getItemId(id: Int): Long = id
  def getView(i: Int, convert: View, parent: ViewGroup): View =
    createView(i, convert, parent)
  def apply(i: Int): T = seq(i)
}

class EmptySeqAdapter[T] extends SeqAdapter[T](Seq.empty[T])({
  (i: Int, convert: View, parent: ViewGroup) => null
})

object SeqAdapter {
  def empty[T] = new EmptySeqAdapter[T]

  object implicits {
    implicit class SeqAdapterMaker[T](val seq: Seq[T]) extends AnyVal {
      def makeAdapter(createView: (Int, View, ViewGroup) => View): SeqAdapter[T] =
        new SeqAdapter[T](seq)(createView)

      def makeAdapterWithTextResource(resource: Int)
                                     (setupView: (T, TextView) => Any)
                                     (implicit context: Context): SeqAdapter[T] =
        new SeqAdapter[T](seq)({ (i: Int, convert: View, parent: ViewGroup) =>
          val row =
            if (convert != null) convert.asInstanceOf[TextView]
            else {
              val inflate = context.systemService[LayoutInflater]
              inflate[TextView](resource, parent)
            }
          setupView(seq(i), row)
          row
        })
    }
  }
}
