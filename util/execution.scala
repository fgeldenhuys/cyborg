package cyborg.util

import cyborg.Context
import android.os.Handler

object execution {
  implicit def fun2runnable(f: => Unit): Runnable = new Runnable { def run() { f } }

  def runnable(f: => Unit): Runnable =
    new Runnable {
      def run() { f }
    }

  def runOnMainLooper(f: => Unit)(implicit context: Context) {
    (new Handler(context.getMainLooper)).post(f)
  }
}
