package cyborg

import android.util.{Log => L}
import java.text.SimpleDateFormat
import java.util.Date
import cyborg.util.numbers.StringNumberConversionsCyborgExt._

trait LogBase {
  def $d(message: => String)
  def $i(message: => String)
  def $w(message: => String)
  def $e(message: => String)
}

trait Log extends LogBase {
  val localTag: Option[String] = None

  def $d(message: => String) { Log.$d(message, localTag) }
  def $i(message: => String) { Log.$i(message, localTag) }
  def $w(message: => String) { Log.$w(message, localTag) }
  def $e(message: => String) { Log.$e(message, localTag) }
}

object Log {
  var globalTag = "cyborg"
  var showDebugInfo = true
  val TimeFormat = new SimpleDateFormat("yyMMdd.HHmmss.SSS")
  val ParseAnon = """\.([\w\d_]+)\$\$anonfun\$([\w\d_]+)\$""".r

  def debugInfo: String =
    if (showDebugInfo) {
      val time = TimeFormat.format(new Date())
      val st = new Throwable().getStackTrace
      st.find { ste =>
        val fn = ste.getFileName
        val cn = ste.getClassName
        val mn = ste.getMethodName
        !cn.startsWith("java.") &&
          !cn.startsWith("scala.") &&
          fn != "Log.scala" &&
          fn != "Log.java" &&
          !mn.startsWith("$")
      } map { ste =>
        val fn = ste.getFileName
        val cn = ste.getClassName
        val mn = ste.getMethodName
        //L.d("wmgc", ste.toString)
        L.d("wmgc", s"cn=$cn mn=$mn")
        val context: Option[String] =
          if (fn.endsWith(".java")) {
            Some(cn.substring(cn.lastIndexOf(".") + 1) + " " + mn)
          }
          else if (fn.endsWith(".scala")) {
            Some(if (cn contains "$$") {
              val tmp = cn.split("\\$\\$").map(_.split("\\$").takeRight(2))
              val method = tmp.filter(_.last.isInt).map(_.head).reverse.dropWhile(_ == "apply").reverse.mkString(" ")
              val cls = tmp.head.head.substring(tmp.head.lastIndexOf(".") + 1)
              cls + " " + method
            }
            else {
              if (mn.contains("$$"))
                cn.substring(cn.lastIndexOf(".") + 1) + " " + mn.substring(mn.lastIndexOf("$$") + 2)
              else
                cn.substring(cn.lastIndexOf(".") + 1) + " " + mn
            })
            /*
            Some(ParseAnon findFirstIn cn match {
              case Some(ParseAnon(cls, fun)) =>
                cls + "." + fun
              case None =>
                if (mn.contains("$$"))
                  cn.substring(cn.lastIndexOf(".") + 1) + "." + mn.substring(mn.lastIndexOf("$$") + 2)
                else
                  cn.substring(cn.lastIndexOf(".") + 1) + "." + mn
            })*/
          }
          else None
        context map ( str => s"$time [$str]" ) getOrElse time
      } getOrElse time
    }
    else ""

  def makeTag(localTag: Option[String]) = localTag.map(globalTag + "-" + _).orElse(Some(globalTag)).get
  def makeMessage(message: String) = debugInfo + " " + message

  def $d(message: => String, tag: Option[String] = None) { L.d(makeTag(tag), makeMessage(message)) }
  def $i(message: => String, tag: Option[String] = None) { L.i(makeTag(tag), makeMessage(message)) }
  def $w(message: => String, tag: Option[String] = None) { L.w(makeTag(tag), makeMessage(message)) }
  def $e(message: => String, tag: Option[String] = None) { L.e(makeTag(tag), makeMessage(message)) }

  def $d(message: => String, tag: String) { L.d(makeTag(Some(tag)), makeMessage(message)) }
  def $i(message: => String, tag: String) { L.i(makeTag(Some(tag)), makeMessage(message)) }
  def $w(message: => String, tag: String) { L.w(makeTag(Some(tag)), makeMessage(message)) }
  def $e(message: => String, tag: String) { L.e(makeTag(Some(tag)), makeMessage(message)) }

  def java_$d(message: String) { L.d(globalTag, makeMessage(message)) }
  def java_$i(message: String) { L.i(globalTag, makeMessage(message)) }
  def java_$w(message: String) { L.w(globalTag, makeMessage(message)) }
  def java_$e(message: String) { L.e(globalTag, makeMessage(message)) }

  def java_$d(message: String, tag: String) { L.d(makeTag(Some(tag)), makeMessage(message)) }
  def java_$i(message: String, tag: String) { L.i(makeTag(Some(tag)), makeMessage(message)) }
  def java_$w(message: String, tag: String) { L.w(makeTag(Some(tag)), makeMessage(message)) }
  def java_$e(message: String, tag: String) { L.e(makeTag(Some(tag)), makeMessage(message)) }

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
}
