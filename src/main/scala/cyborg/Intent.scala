package cyborg

import cyborg.Context._
import android.content.Intent

object Intent {
  trait ExtraProp[T] {
    def get(intent: android.content.Intent, key: String): Option[T]
    def set(intent: android.content.Intent, key: String, value: T)
  }
  implicit val stringExtraProp = new ExtraProp[String] {
    def get(intent: Intent, key: String): Option[String] =
      for (extras <- intent.extras; value <- Option(extras.getString(key))) yield value
    def set(intent: Intent, key: String, value: String): Unit =
      for (extras <- intent.extras) extras.putString(key, value)
  }
  implicit val intExtraProp = new ExtraProp[Int] {
    def get(intent: Intent, key: String): Option[Int] =
      for (extras <- intent.extras; value <- Option(extras.getInt(key))) yield value
    def set(intent: Intent, key: String, value: Int): Unit =
      for (extras <- intent.extras) extras.putInt(key, value)
  }

  implicit class IntentExt(val self: android.content.Intent) /* extends AnyVal */ { // Nested class not allowed
    def start(implicit activity: Activity) { activity.startActivity(self) }

    def startForResult(requestCode: Int)(implicit activity: Activity) {
      activity.startActivityForResult(self, requestCode)
    }

    class Extra {
      def apply[T](key: String)(implicit prop: ExtraProp[T]): Option[T] = {
        prop.get(self, key)
      }
      def update[T](key: String, value: T)(implicit prop: ExtraProp[T]) =
        prop.set(self, key, value)
    }
    val extra = new Extra

    def putExtras(extras: (String, Any)*) {
      for ((name, value) <- extras) {
        value match {
          case string: String => self.putExtra(name, string)
          case int: Int => self.putExtra(name, int)
          case double: Double => self.putExtra(name, double)
          case boolean: Boolean => self.putExtra(name, boolean)
          case byteArray: Array[Byte] => self.putExtra(name, byteArray)
          case _ => throw Intent.UnknownExtraTypeException(name, value)
        }
      }
    }

    def extras = Option(self.getExtras)
    def stringExtra(name: String) = Option(self.getStringExtra(name))
  }

  object Intent {
    case class UnknownExtraTypeException(name: String, value: Any)
      extends Exception(s"Unknown extra type: $name -> ${value.toString}")

    def apply() = new android.content.Intent()
    def apply[A](implicit context: Context, m: Manifest[A]) = new android.content.Intent(context, /* m.runtimeClass*/ m.erasure)
  }
}
