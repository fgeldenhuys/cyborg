package cyborg

import android.util.{Log => L}
import java.text.SimpleDateFormat
import java.util.Date
import cyborg.util.numbers.StringNumberConversionsCyborgExt._
import scalaz._

trait LogBase {
  def $d(message: => String): Unit
  def $i(message: => String): Unit
  def $w(message: => String): Unit
  def $e(message: => String): Unit
}

trait Log extends LogBase {
  val localTag: Option[String] = None

  def $d(message: => String): Unit = { Log.$d(message, localTag) }
  def $i(message: => String): Unit = { Log.$i(message, localTag) }
  def $w(message: => String): Unit = { Log.$w(message, localTag) }
  def $e(message: => String): Unit = { Log.$e(message, localTag) }
}

object Log extends LogBase {
  var globalTag = "cyborg"
  var showDebugInfo = true
  val TimeFormat = new SimpleDateFormat("yyMMdd.HHmmss.SSS")

  def debugInfo(drop: Int = 0): String =
    if (showDebugInfo) {
      val time = TimeFormat.format(new Date())
      val st = new Throwable().getStackTrace
      val i = st.indexWhere { ste =>
        val fn = ste.getFileName
        val cn = ste.getClassName
        val mn = ste.getMethodName
        !cn.startsWith("java.") &&
          !cn.startsWith("scala.") &&
          fn != "Log.scala" &&
          fn != "Log.java" &&
          !mn.startsWith("$")
      } + drop
      if (st.isDefinedAt(i)) {
        val ste = st(i)
        val fn = ste.getFileName
        val cn = ste.getClassName
        val mn = ste.getMethodName
        //L.d("wmgc", s"cn=$cn mn=$mn")
        val context: Option[String] =
          if (fn.endsWith(".java")) {
            Some(cn.substring(cn.lastIndexOf(".") + 1) + " " + mn)
          }
          else if (fn.endsWith(".scala")) {
            Some(if (cn contains "$$") {
              val tmp = cn.split("\\$\\$").map(_.split("\\$").takeRight(2))
              val method = tmp.filter(_.last.isInt).map(_.head).reverse.dropWhile(_ == "apply").reverse.mkString(" ", " ", "")
              val pkg = tmp.head.head
              val cls = pkg.substring(pkg.lastIndexOf(".") + 1)
              cls + method
            }
            else {
              if (mn.contains("$$"))
                cn.substring(cn.lastIndexOf(".") + 1) + " " + mn.substring(mn.lastIndexOf("$$") + 2)
              else
                cn.substring(cn.lastIndexOf(".") + 1) + " " + mn
            })
          }
          else None
        context map ( str => s"$time [$str]" ) getOrElse time
      } else time
    }
    else ""

  def makeTag(localTag: Option[String]) = localTag.map(globalTag + "-" + _).orElse(Some(globalTag)).get
  def makeMessage(message: String, discard: Int = 0) = debugInfo(discard) + " " + message

  def $d(message: => String): Unit = { $d(message, None) }
  def $i(message: => String): Unit = { $i(message, None) }
  def $w(message: => String): Unit = { $w(message, None) }
  def $e(message: => String): Unit = { $e(message, None) }

  def $_d(message: => String): Unit = { L.d(makeTag(None), message) }
  def $_i(message: => String): Unit = { L.i(makeTag(None), message) }
  def $_w(message: => String): Unit = { L.w(makeTag(None), message) }
  def $_e(message: => String): Unit = { L.e(makeTag(None), message) }

  def $d(message: => String, tag: Option[String]): Unit = { L.d(makeTag(tag), makeMessage(message)) }
  def $i(message: => String, tag: Option[String]): Unit = { L.i(makeTag(tag), makeMessage(message)) }
  def $w(message: => String, tag: Option[String]): Unit = { L.w(makeTag(tag), makeMessage(message)) }
  def $e(message: => String, tag: Option[String]): Unit = { L.e(makeTag(tag), makeMessage(message)) }

  def $d(message: => String, tag: String): Unit = { L.d(makeTag(Some(tag)), makeMessage(message)) }
  def $i(message: => String, tag: String): Unit = { L.i(makeTag(Some(tag)), makeMessage(message)) }
  def $w(message: => String, tag: String): Unit = { L.w(makeTag(Some(tag)), makeMessage(message)) }
  def $e(message: => String, tag: String): Unit = { L.e(makeTag(Some(tag)), makeMessage(message)) }

  def $d(message: => String, drop: Int): Unit = { L.d(makeTag(None), makeMessage(message, drop)) }
  def $i(message: => String, drop: Int): Unit = { L.i(makeTag(None), makeMessage(message, drop)) }
  def $w(message: => String, drop: Int): Unit = { L.w(makeTag(None), makeMessage(message, drop)) }
  def $e(message: => String, drop: Int): Unit = { L.e(makeTag(None), makeMessage(message, drop)) }

  def java_$d(message: String): Unit = { L.d(globalTag, makeMessage(message)) }
  def java_$i(message: String): Unit = { L.i(globalTag, makeMessage(message)) }
  def java_$w(message: String): Unit = { L.w(globalTag, makeMessage(message)) }
  def java_$e(message: String): Unit = { L.e(globalTag, makeMessage(message)) }

  def java_$d(message: String, tag: String): Unit = { L.d(makeTag(Some(tag)), makeMessage(message)) }
  def java_$i(message: String, tag: String): Unit = { L.i(makeTag(Some(tag)), makeMessage(message)) }
  def java_$w(message: String, tag: String): Unit = { L.w(makeTag(Some(tag)), makeMessage(message)) }
  def java_$e(message: String, tag: String): Unit = { L.e(makeTag(Some(tag)), makeMessage(message)) }

  /*implicit class WithLogMethods(string: String) {
    def log$d(pre: => String = "", tag: Option[String] = None) { $d(pre + string, tag) }
    def log$i(pre: => String = "", tag: Option[String] = None) { $i(pre + string, tag) }
    def log$w(pre: => String = "", tag: Option[String] = None) { $w(pre + string, tag) }
    def log$e(pre: => String = "", tag: Option[String] = None) { $e(pre + string, tag) }

    def log$d(pre: => String, tag: String) { $d(pre + string, tag) }
    def log$i(pre: => String, tag: String) { $i(pre + string, tag) }
    def log$w(pre: => String, tag: String) { $w(pre + string, tag) }
    def log$e(pre: => String, tag: String) { $e(pre + string, tag) }
  }*/

  implicit class EitherWithLogging[R](val v: Throwable \/ R) extends AnyVal {
    def traceLeft: Throwable \/ R = {
      v.fold({ l => $w(l.getStackTraceString) }, identity)
      v
    }

    def traceLeft(message: String): Throwable \/ R = {
      v.fold({ l => $w(message + ":\n" + l.getStackTraceString) }, identity)
      v
    }
  }

  implicit class LogAnything[A](val a: A) extends AnyRef {
    def printToLog(): A = { $d(a.toString); a }
    def printToLog(f: A => String): A = { $d(f(a)); a }
  }
}
