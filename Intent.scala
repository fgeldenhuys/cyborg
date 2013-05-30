package cyborg

import cyborg.Context._

class Intent(val self: android.content.Intent) /*extends AnyVal*/ {
  def start(implicit activity: Activity) { activity.startActivity(self) }
  def start(activity: android.app.Activity) { activity.startActivity(self) }

  def startForResult(requestCode: Int)(implicit activity: Activity) {
    activity.startActivityForResult(self, requestCode)
  }
  def startForResult(requestCode: Int, activity: android.app.Activity) {
    activity.startActivityForResult(self, requestCode)
  }

  def update(name: String, value: String) = self.putExtra(name, value)
  def update(name: String, value: Int) = self.putExtra(name, value)
}

object Intent {
  implicit def android2cyborgIntent(intent: android.content.Intent) = new Intent(intent)
  implicit def cyborg2androidIntent(intent: Intent) = intent.self
  def apply() = new android.content.Intent()
  def apply[A](implicit context: Context, m: Manifest[A]) = new android.content.Intent(context, /* m.runtimeClass*/ m.erasure)
}
