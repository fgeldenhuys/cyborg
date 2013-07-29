package cyborg.util

import android.os.Debug
import cyborg.Log._

object debug {
  /*implicit class Tapper[A](what: A) {
    def tap(f: (A) => Any): A = {
      f(what)
      what
    }
  }*/

  def printStackTrace() {
    $d((new Throwable()).getStackTrace.drop(2).mkString("\n>  "))
  }

  def printMemoryUsage() {
    val meminfo = new Debug.MemoryInfo
    Debug.getMemoryInfo(meminfo)
    $d("Memory Usage: Pss=" + meminfo.getTotalPss + " Private=" + meminfo.getTotalPrivateDirty + " Shared=" + meminfo.getTotalSharedDirty())
  }
}
