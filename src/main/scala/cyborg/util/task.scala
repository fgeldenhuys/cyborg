package cyborg.util

import scalaz._
import scalaz.concurrent._
import cyborg.Activity

object task {

  implicit class TaskOnUiThreadExt[A](val t: Task[A]) extends AnyVal {
    def runAsyncUI(f: (Throwable \/ A) => Unit)(implicit a: Activity) {
      a.runOnUiThread {
        t.runAsync(f)
      }
    }

    def runAsyncUI()(implicit a: Activity): Unit = runAsyncUI { _ => Unit }
  }

}
