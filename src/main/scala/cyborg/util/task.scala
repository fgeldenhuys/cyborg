package cyborg.util

import scalaz._
import scalaz.concurrent._
import cyborg.Activity

object task {

  // This doesn't work as I expected
  /*implicit class TaskOnUiThreadExt[A](val t: Task[A]) extends AnyVal {
    def runAsyncUI(f: (Throwable \/ A) => Unit)(implicit a: Activity) {
      a runOnUiThread {
        f(t.attemptRun)
      }
    }
  }*/

}
