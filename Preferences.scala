package cyborg

object Preferences {
  case class Preferences(section: String) {
    def apply[T](key: String)(implicit prop: Prop[T], context: Context): Option[T] =
      prop.get(section, key)
    def update[T](key: String, value: T)(implicit prop: Prop[T], context: Context) {
      prop.set(section, key, value)
    }
    def delete[T](key: String)(implicit prop: Prop[T], context: Context) {
      prop.del(section, key)
    }
    def raw(implicit context: Context) = context.getSharedPreferences(section, 0)
    def java(implicit context: android.content.Context): JavaPreferences =
      new JavaPreferences(context, section)
  }

  trait Prop[T] {
    def prefs(section: String)(implicit context: Context) =
      context.getSharedPreferences(section, 0)
    def get(section: String, key: String)(implicit context: Context): Option[T]
    def set(section: String, key: String, value: T)(implicit context: Context)
    def del(section: String, key: String)(implicit context: Context) {
      prefs(section).edit().remove(key).apply()
    }
  }

  implicit val intProp = new Prop[Int] {
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

  implicit val stringProp = new Prop[String] {
    def get(section: String, key: String)(implicit context: Context): Option[String] = {
      Option(prefs(section).getString(key, null))
    }
    def set(section: String, key: String, value: String)(implicit context: Context) {
      prefs(section).edit().putString(key, value).apply()
    }
  }

  implicit val booleanProp = new Prop[Boolean] {
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

  implicit val longProp = new Prop[Long] {
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
