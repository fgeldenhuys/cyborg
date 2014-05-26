package cyborg

import android.content.{Intent => AIntent}
import android.app.{Activity => AActivity}
import android.os.Parcelable
import cyborg.Context._
import cyborg.util.control._
import scala.collection.JavaConversions._
import scalaz._, Scalaz._, scalaz.concurrent._

object Intent {
  trait ExtraProp[T] {
    def get(intent: AIntent, key: String): Option[T]
    def set(intent: AIntent, key: String, value: T)
  }

  def makeParcelableListExtraProp[T <: Parcelable](implicit prop: ExtraProp[T]): ExtraProp[List[T]] = new ExtraProp[List[T]] {
    override def get(intent: AIntent, key: String): Option[List[T]] =
      for {
        extras <- intent.extras
        arrayList <- tryOption(extras.getParcelableArrayList[T](key))
      } yield arrayList.toList
    override def set(intent: AIntent, key: String, value: List[T]) {
      intent.putExtra(key, new java.util.ArrayList[T](value))
    }
  }

  implicit val stringExtraProp = new ExtraProp[String] {
    def get(intent: AIntent, key: String): Option[String] =
      for (extras <- intent.extras; value <- Option(extras.getString(key))) yield value
    def set(intent: AIntent, key: String, value: String): Unit = intent.putExtra(key, value)
  }
  implicit val intExtraProp = new ExtraProp[Int] {
    def get(intent: AIntent, key: String): Option[Int] =
      for (extras <- intent.extras; value <- Option(extras.getInt(key))) yield value
    def set(intent: AIntent, key: String, value: Int): Unit = intent.putExtra(key, value)
  }
  implicit val longExtraProp = new ExtraProp[Long] {
    def get(intent: AIntent, key: String): Option[Long] =
      for (extras <- intent.extras; value <- Option(extras.getLong(key))) yield value
    def set(intent: AIntent, key: String, value: Long): Unit = intent.putExtra(key, value)
  }
  implicit val booleanExtraProp = new ExtraProp[Boolean] {
    def get(intent: AIntent, key: String): Option[Boolean] =
      for (extras <- intent.extras; value <- Option(extras.getBoolean(key))) yield value
    def set(intent: AIntent, key: String, value: Boolean): Unit = intent.putExtra(key, value)
  }

  implicit class IntentExt(val self: AIntent) /* extends AnyVal */ { // Nested class not allowed
    def start(implicit activity: Activity) { activity.startActivity(self) }

    //def startForResult: Future((Int, Intent) => )

    def startForResult(requestCode: Int)(implicit activity: Activity) {
      activity.startActivityForResult(self, requestCode)
    }

    def broadcast()(implicit context: Context) {
      context.sendBroadcast(self)
    }

    def makeResult(resultCode: Int)(implicit activity: Activity): AIntent = {
      activity.setResult(resultCode, self)
      self
    }

    def ->:[A](pair: (String, A))(implicit prop: ExtraProp[A]): AIntent = {
      prop.set(self, pair._1, pair._2)
      self
    }

    class Extra {
      def apply[T](key: String)(implicit prop: ExtraProp[T]): Option[T] =
        prop.get(self, key)
      def update[T](key: String, value: T)(implicit prop: ExtraProp[T]) =
        prop.set(self, key, value)
    }
    val extra = new Extra

    @deprecated def putExtras(extras: (String, Any)*) {
      for ((name, value) <- extras) {
        value match {
          case string: String => self.putExtra(name, string)
          case int: Int => self.putExtra(name, int)
          case long: Long => self.putExtra(name, long)
          case double: Double => self.putExtra(name, double)
          case boolean: Boolean => self.putExtra(name, boolean)
          case byteArray: Array[Byte] => self.putExtra(name, byteArray)
          case parcel: Parcelable => self.putExtra(name, parcel)
          case _ => throw Intent.UnknownExtraTypeException(name, value)
        }
      }
    }

    def extras = Option(self.getExtras)
  }

  object Intent {
    case class UnknownExtraTypeException(name: String, value: Any)
      extends Exception(s"Unknown extra type: $name -> ${value.toString}")

    def apply() = new AIntent()
    def apply[A](implicit context: Context, m: Manifest[A]) = new AIntent(context, /* m.runtimeClass*/ m.erasure)
    def apply(action: String) = new AIntent(action)
  }
}
