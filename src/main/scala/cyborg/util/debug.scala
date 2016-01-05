package cyborg.util

import java.util.concurrent.{ScheduledFuture, TimeUnit}
import android.os.Debug
import cyborg.Log._
import cyborg.util.execution.ScheduledExecutionContext
import scala.collection.mutable
import cyborg.util.binary.Bytes

object debug {
  /*implicit class Tapper[A](what: A) {
    def tap(f: (A) => Any): A = {
      f(what)
      what
    }
  }*/

  def getStackTrace: String =
    new Throwable().getStackTrace.drop(2).mkString("\n>  ")

  def printStackTrace(): Unit = {
    $d(getStackTrace)
  }

  // Handy for timing something that must run on the current thread
  def warnAfterTime[A](millis: Long)(f: => A)(implicit sec: ScheduledExecutionContext): A = {
    val trace = getStackTrace
    val sentinel = Option(sec).map(_.schedule(new Runnable {
      override def run(): Unit = {
        $w("WARN AFTER TIME: " + trace)
      }
    }, millis, TimeUnit.MILLISECONDS))
    val result = f
    sentinel.foreach(_.cancel(false))
    result
  }

  def warnAfterTimeWith[A](millis: Long, warning: String)(f: => A)(implicit sec: ScheduledExecutionContext): A = {
    val sentinel = sec.schedule(new Runnable {
      override def run(): Unit = {
        $w("WARN AFTER TIME: " + warning)
      }
    }, millis, TimeUnit.MILLISECONDS)
    val result = f
    sentinel.cancel(false)
    result
  }

  def warnOnUiThread: Boolean = {
    if (android.os.Looper.myLooper() == android.os.Looper.getMainLooper) {
      $w("WARNING this should not be on the UI thread: " + getStackTrace)
      true
    }
    else false
  }

  def warnIfNotOnUiThread: Boolean = {
    if (android.os.Looper.myLooper() != android.os.Looper.getMainLooper) {
      $w("WARNING this should only be on the UI thread: " + getStackTrace)
      true
    }
    else false
  }

  def printMemoryUsage(): Unit = {
    val meminfo = new Debug.MemoryInfo
    Debug.getMemoryInfo(meminfo)
    $d("Memory Usage: Pss=" + meminfo.getTotalPss + " Private=" + meminfo.getTotalPrivateDirty + " Shared=" + meminfo.getTotalSharedDirty())
  }

  case class MemoryUsageValues(pss: Bytes, priv: Bytes, shared: Bytes)

  def getMemoryUsage: MemoryUsageValues = {
    val meminfo = new Debug.MemoryInfo
    Debug.getMemoryInfo(meminfo)
    MemoryUsageValues(Bytes(meminfo.getTotalPss * 1024),
      Bytes(meminfo.getTotalPrivateDirty * 1024),
      Bytes(meminfo.getTotalSharedDirty * 1024))
  }

  def killRAM(): Unit = {
    val MB16 = 16 * 1024 * 1024
    val junk = mutable.ListBuffer.empty[Array[Byte]]
    while (true) {
      //val chunk = Array.fill(MB16)(0.toByte)
      val chunk = Array.ofDim[Byte](MB16)
      junk += chunk
      val meminfo = new Debug.MemoryInfo
      Debug.getMemoryInfo(meminfo)
      val wasted = Bytes(junk.length * MB16)
      val pss = Bytes(meminfo.getTotalPss * 1024)
      val priv = Bytes(meminfo.getTotalPrivateDirty * 1024)
      val shared = Bytes(meminfo.getTotalSharedDirty * 1024)
      $d(s"$wasted wasted, pss=$pss private=$priv shared=$shared")
    }
  }
}