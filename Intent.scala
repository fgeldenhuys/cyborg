package cyborg

import cyborg.Context._

object Intent {
  implicit class IntentExt(val self: android.content.Intent) extends AnyVal {
    def start(implicit activity: Activity) { activity.startActivity(self) }

    def startForResult(requestCode: Int)(implicit activity: Activity) {
      activity.startActivityForResult(self, requestCode)
    }

    def update(name: String, value: String) = self.putExtra(name, value)
    def update(name: String, value: Int) = self.putExtra(name, value)

    def extras = Option(self.getExtras)
    def stringExtra(name: String) = Option(self.getStringExtra(name))
  }

  object Intent {
    def apply() = new android.content.Intent()
    def apply[A](implicit context: Context, m: Manifest[A]) = new android.content.Intent(context, /* m.runtimeClass*/ m.erasure)
  }
}
