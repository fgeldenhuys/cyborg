package cyborg

import android.util.{Log => L}
import java.text.SimpleDateFormat
import java.util.Date

trait Log {
  val localTag: Option[String] = None

  def $d(message: => String) { Log.$d(message, localTag) }
  def $i(message: => String) { Log.$i(message, localTag) }
  def $w(message: => String) { Log.$w(message, localTag) }
  def $e(message: => String) { Log.$e(message, localTag) }
}

object Log {
  var globalTag = "cyborg"
  val timeFormat = new SimpleDateFormat("yyMMdd.HHmmss.SSS")

  def makeTag(localTag: Option[String]) = localTag.map(globalTag + "-" + _).orElse(Some(globalTag)).get
  def makeMessage(message: String) = timeFormat.format(new Date()) + "  " + message

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
}
