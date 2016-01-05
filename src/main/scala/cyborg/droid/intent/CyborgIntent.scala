package cyborg.droid.intent

import android.app.Activity
import android.content.{Context, Intent}

import scalaz._, scalaz.concurrent._, Scalaz._
import scala.concurrent.stm._
import io.dylemma.frp._

import cyborg.droid.CyborgActivity._
import cyborg.util.frp._

trait IntentExtra[A] {
  def get(intent: Intent, key: String): Option[A]
  def set(intent: Intent, key: String, value: A): Unit
}

object CyborgIntentHelpers {
  class ExtraHelper(intent: Intent) {
    def apply[A](key: String)(implicit E: IntentExtra[A]): Option[A] = E.get(intent, key)
    def update[A](key: String, value: A)(implicit E: IntentExtra[A]) = E.set(intent, key, value)
  }
}

class CyborgIntentCallbacks(implicit observer: Observer) {
  val map = TMap[Int, Promise[(ActivityResultCode, Option[Intent])]]()
  val next = Ref[Int](1001)

  def startIntent(activity: Activity, intent: Intent): Promise[(ActivityResultCode, Option[Intent])] = {
    atomic { implicit txn =>
      val requestCode = next.getAndTransform(_ + 1)
      val promise = Promise.emptyPromise[(ActivityResultCode, Option[Intent])]
      map += (requestCode -> promise)
      activity.startActivityForResult(intent, requestCode)
      promise
    }
  }

  def onActivityResult(requestCode: Int, result: ActivityResultCode, data: Intent): Boolean = {
    atomic { implicit txn =>
      map.get(requestCode).map { promise =>
        map -= requestCode
        promise
      }
    } .cata({ promise =>
      promise.fulfill((result, Option(data)))
      true
    }, false)
  }
}

object CyborgIntentCallbacks {
  def apply(events: EventSource[(Int, ActivityResultCode, Intent)])
           (implicit observer: Observer): CyborgIntentCallbacks = {
    val callbacks = new CyborgIntentCallbacks
    events.attemptEach {
      case (requestCode, result, data) => callbacks.onActivityResult(requestCode, result, data)
    }
    callbacks
  }
}

class CyborgIntentExt(val intent: Intent) extends AnyVal {
  def start(activity: Activity): Unit = {
    activity.startActivity(intent)
  }

  def startForResult(activity: Activity, callbacks: CyborgIntentCallbacks):
    Promise[(ActivityResultCode, Option[Intent])] =
      callbacks.startIntent(activity, intent)

  def extra = new CyborgIntentHelpers.ExtraHelper(intent)

  def ->: [A] (pair: (String, A))(implicit E: IntentExtra[A]): Intent = {
    E.set(intent, pair._1, pair._2)
    intent
  }
}

object CyborgIntent {
  implicit def CyborgIntentExtImplicit(x: Intent) = new CyborgIntentExt(x)

  def explicitIntent[A](context: Context)(implicit m: Manifest[A]) = new Intent(context, m.runtimeClass)
  def actionIntent(action: String) = new Intent(action)

  implicit val stringIntentExtra = new IntentExtra[String] {
    def get(intent: Intent, key: String): Option[String] =
      Option(intent.getExtras).flatMap(e => Option(e.getString(key)))
    def set(intent: Intent, key: String, value: String): Unit = intent.putExtra(key, value)
  }
  implicit val intIntentExtra = new IntentExtra[Int] {
    def get(intent: Intent, key: String): Option[Int] =
      Option(intent.getExtras).flatMap(e => Option(e.getInt(key)))
    def set(intent: Intent, key: String, value: Int): Unit = intent.putExtra(key, value)
  }
  implicit val longIntentExtra = new IntentExtra[Long] {
    def get(intent: Intent, key: String): Option[Long] =
      Option(intent.getExtras).flatMap(e => Option(e.getLong(key)))
    def set(intent: Intent, key: String, value: Long): Unit = intent.putExtra(key, value)
  }
  implicit val booleanIntentExtra = new IntentExtra[Boolean] {
    def get(intent: Intent, key: String): Option[Boolean] =
      Option(intent.getExtras).flatMap(e => Option(e.getBoolean(key)))
    def set(intent: Intent, key: String, value: Boolean): Unit = intent.putExtra(key, value)
  }
  implicit val byteArrayIntentExtra = new IntentExtra[Array[Byte]] {
    def get(intent: Intent, key: String): Option[Array[Byte]] =
      Option(intent.getExtras).flatMap(e => Option(e.getByteArray(key)))
    def set(intent: Intent, key: String, value: Array[Byte]): Unit = intent.putExtra(key, value)
  }
}
