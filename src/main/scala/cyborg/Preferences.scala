package cyborg

import cyborg.Context._
import scala.collection.JavaConversions._

object Preferences {
  case class WrongPrefTypeException(message: String) extends Exception(message)

  case class Preferences(section: String) {
    private def prefs(implicit context: Context) =
      context.getSharedPreferences(section, Context.ModeMultiProcess)

    def apply[T](key: String)(implicit prop: PreferencesProp[T], context: Context): Option[T] =
      prop.get(section, key)

    def update[T](key: String, value: T)(implicit prop: PreferencesProp[T], context: Context) {
      prop.set(section, key, value)
    }

    def delete(key: String)(implicit context: Context) {
      prefs.edit().remove(key).apply()
    }

    def setOrDelete[T](key: String, value: Option[T])(implicit prop: PreferencesProp[T], context: Context) {
      value map (prop.set(section, key, _)) getOrElse delete(key)
    }

    def makeDefault[T](key: String, value: T)(implicit prop: PreferencesProp[T], context: Context) {
      if (prop.get(section, key).isEmpty) prop.set(section, key, value)
    }

    def ? (key: String)(implicit prop: PreferencesProp[Boolean], context: Context): Boolean =
      apply[Boolean](key) getOrElse false

    def addString(key: String, value: String)(implicit context: Context) {
      if (prefs.contains(key)) {
        val set = prefs.getStringSet(key, null)
        if (set == null) throw WrongPrefTypeException("Expected string set")
        val newSet = set + value
        prefs.edit().putStringSet(key, newSet).apply()
      }
      else {
        val set = Set[String](value)
        prefs.edit().putStringSet(key, set).apply()
      }
    }

    def raw(implicit context: Context) = context.getSharedPreferences(section, Context.ModeMultiProcess)

    def java(implicit context: android.content.Context): JavaPreferences =
      new JavaPreferences(context, section)
  }

  trait PreferencesProp[T] {
    def prefs(section: String)(implicit context: Context) =
      context.getSharedPreferences(section, Context.ModeMultiProcess)
    def get(section: String, key: String)(implicit context: Context): Option[T]
    def set(section: String, key: String, value: T)(implicit context: Context)
  }

  implicit val intProp = new PreferencesProp[Int] {
    def get(section: String, key: String)(implicit context: Context): Option[Int] = {
      if (prefs(section).contains(key))
        Some(prefs(section).getInt(key, 0))
      else
        None
    }
    def set(section: String, key: String, value: Int)(implicit context: Context) {
      prefs(section).edit().putInt(key, value).apply()
    }
  }

  implicit val stringProp = new PreferencesProp[String] {
    def get(section: String, key: String)(implicit context: Context): Option[String] = {
      Option(prefs(section).getString(key, null))
    }
    def set(section: String, key: String, value: String)(implicit context: Context) {
      prefs(section).edit().putString(key, value).apply()
    }
  }

  implicit val booleanProp = new PreferencesProp[Boolean] {
    def get(section: String, key: String)(implicit context: Context): Option[Boolean] = {
      if (prefs(section).contains(key))
        Some(prefs(section).getBoolean(key, false))
      else
        None
    }
    def set(section: String, key: String, value: Boolean)(implicit context: Context) {
      prefs(section).edit().putBoolean(key, value).apply()
    }
  }

  implicit val longProp = new PreferencesProp[Long] {
    def get(section: String, key: String)(implicit context: Context): Option[Long] = {
      if (prefs(section).contains(key))
        Some(prefs(section).getLong(key, 0))
      else
        None
    }
    def set(section: String, key: String, value: Long)(implicit context: Context) {
      prefs(section).edit().putLong(key, value).apply()
    }
  }

  implicit val stringSetProp = new PreferencesProp[Set[String]] {
    def get(section: String, key: String)(implicit context: Context): Option[Set[String]] = {
      if (prefs(section).contains(key))
        Some(prefs(section).getStringSet(key, null).toSet)
      else None
    }
    def set(section: String, key: String, value: Set[String])(implicit context: Context) {
      prefs(section).edit().putStringSet(key, value)
    }
  }

  class JavaPreferences(androidContext: android.content.Context, section: String) {
    val prefs = new Preferences(section)
    implicit val context: Context = androidContext

    // For Java:
    def getInt(key: String, default: Int): Int =
      prefs[Int](key) getOrElse default
    def getString(key: String, default: String): String =
      prefs[String](key) getOrElse default
    def getBoolean(key: String, default: Boolean): Boolean =
      prefs[Boolean](key) getOrElse default
    def getLong(key: String, default: Long): Long =
      prefs[Long](key) getOrElse default

    def setInt(key: String, value: Int) { prefs.update(key, value) }
    def setString(key: String, value: String) { prefs.update(key, value) }
    def setBoolean(key: String, value: Boolean) { prefs.update(key, value) }
    def setLong(key: String, value: Long) { prefs.update(key, value) }
  }
}
