package cyborg.util

import android.os.Debug
import cyborg.Log._
import scala.collection.mutable
import cyborg.util.binary.Bytes

object debug {
  /*implicit class Tapper[A](what: A) {
    def tap(f: (A) => Any): A = {
      f(what)
      what
    }
  }*/

  def getStackTrace: String = {
    new Throwable().getStackTrace.drop(2).mkString("\n>  ")
  }

  def printStackTrace() {
    $d(getStackTrace)
  }

  def printMemoryUsage() {
    val meminfo = new Debug.MemoryInfo
    Debug.getMemoryInfo(meminfo)
    $d("Memory Usage: Pss=" + meminfo.getTotalPss + " Private=" + meminfo.getTotalPrivateDirty + " Shared=" + meminfo.getTotalSharedDirty())
  }

  def killRAM() {
    val MB16 = 16 * 1024 * 1024
    val junk = mutable.ListBuffer.empty[Array[Byte]]
    while (true) {
      //val chunk = Array.fill(MB16)(0.toByte)
      val chunk = Array.ofDim[Byte](MB16)
      junk += chunk
      val meminfo = new Debug.MemoryInfo
      Debug.getMemoryInfo(meminfo)
      val wasted = Bytes(junk.length * MB16)
      val pss = Bytes(meminfo.getTotalPss)
      val priv = Bytes(meminfo.getTotalPrivateDirty)
      val shared = Bytes(meminfo.getTotalSharedDirty)
      $d(s"$wasted wasted, pss=$pss private=$priv shared=$shared")
    }
  }
}
