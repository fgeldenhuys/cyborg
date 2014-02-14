package cyborg.preferences

import android.content.SharedPreferences
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import cyborg.Context, cyborg.Context._
import cyborg.db.SQLite._
import cyborg.util.control._
import cyborg.Log._
import scalaz._, Scalaz._

object SqlitePreferences {
  case class KeyNotDefinedException(section: String, key: String) extends Exception(s"Key not set '$key' for section '$section'")
  case class NotASetException(section: String, key: String) extends Exception(s"Key '$key' is not a set in section '$section'")

  class Preferences(val section: String, val androidPrefsSection: String)(implicit val context: Context) {
    val helper = new DbOpenHelper
    val androidPrefs: Option[SharedPreferences] = {
      assert(context != null)
      assert(context.c != null)
      assert(androidPrefsSection != null)
      tryOption {
        Option(context.getSharedPreferences(androidPrefsSection, Context.ModeMultiProcess))
      }.flatten
    }

    def apply[T](key: String)(implicit prop: PrefProp[T]): Option[T] = {
      retryOnException[Option[T]](3) {
        helper.readableDatabase.toOption.flatMap(db => prop.get(db, section, key))
          .orElse(androidPrefs.flatMap(ap => prop.getAndroidPref(ap, key)))
      } .toOption.flatten
    }

    def update[T](key: String, value: T)(implicit prop: PrefProp[T]) {
      retryOnException[Any](3) {
        helper.writableDatabase.map(db => prop.put(db, section, key, value))
      }
    }

    def ? (key: String)(implicit prop: PrefProp[Boolean]): Boolean = {
      retryOnException[Boolean](3) {
        helper.readableDatabase.map(db => prop.?(db, section, key)).getOrElse(false)
      } .getOrElse(false)
    }

    def delete(key: String) {
      retryOnException[Any](3) {
        helper.writableDatabase.map { db =>
          db.delete("prime", "section = ? AND key = ?", section, key)
        }
      }
    }

    def setOrDelete[T](key: String, value: Option[T])(implicit prop: PrefProp[T]) {
      value map (v => update[T](key, v)) getOrElse delete(key)
    }

    def makeDefault[T](key: String, value: T)(implicit prop: PrefProp[T]) {
      if (apply[T](key).isEmpty) update[T](key, value)
    }

    def increment[T](key: String)(implicit prop: PrefProp[T]): Option[T] = {
      tryOption {
        helper.writableDatabase.toOption.flatMap(db => prop.increment(db, section, key))
      } .flatMap(identity)
    }

    class PrefSet(val key: String) {
      def += [T](value: T)(implicit prop: PrefProp[T]) {
        helper.writableDatabase.map(db => prop.setAdd(db, section, key, value))
      }

      def -= [T](value: T)(implicit prop: PrefProp[T]) {
        helper.writableDatabase.map(db => prop.setRemove(db, section, key, value))
      }

      def delete() {
        helper.writableDatabase.map {
          _.transaction { db =>
            val check = db.raw("SELECT value, setValue FROM prime WHERE section = ? AND key = ?", section, key)
            if (check.isEmpty) { // Nothing there
              //throw KeyNotDefinedException(section, key)
              $w("Key not defined! " + cyborg.util.debug.getStackTrace)
            }
            else if (check.get[Int]("setValue").exists(_ == 0)) { // Value defined, but not a set
              //throw NotASetException(section, key)
              $w("Not a set! " + cyborg.util.debug.getStackTrace)
            }
            else {
              check.get[Long]("value") map { setId =>
                db.delete("sets", "section = ? AND setId = ?", section, setId)
                db.delete("prime", "section = ? AND key = ?", section, key)
              }
            }
            check.close()
          }
        }
      }

      def toList[T](implicit prop: PrefProp[T]): List[T] = {
        helper.readableDatabase.map(db => prop.setGet(db, section, key)).getOrElse(List.empty)
      }
      def toSet[T](implicit prop: PrefProp[T]): Set[T] = toList[T](prop).toSet
    }

    def set(key: String) = new PrefSet(key)

    class DbOpenHelper extends SQLiteOpenHelper(context, "CyborgPreferencesDb", null, 1) {
      def onCreate(db: SQLiteDatabase) {
        db.exec(
          """
            | CREATE TABLE IF NOT EXISTS prime (
            |   id INTEGER PRIMARY KEY NOT NULL,
            |   section TEXT NOT NULL,
            |   key TEXT NOT NULL,
            |   value BLOB NOT NULL,
            |   setValue INTEGER DEFAULT 0,
            |   UNIQUE(section, key) ON CONFLICT REPLACE
            | );
          """.stripMargin)
        db.exec(
          """
            | CREATE TABLE IF NOT EXISTS meta (
            |   property TEXT PRIMARY KEY,
            |   value BLOB NOT NULL
            | );
          """.stripMargin)
        db.exec(
          """
            | CREATE TABLE IF NOT EXISTS sets (
            |   id INTEGER PRIMARY KEY NOT NULL,
            |   section TEXT NOT NULL,
            |   setId INTEGER NOT NULL,
            |   value BLOB NOT NULL
            | );
          """.stripMargin)
        db.insert("meta", "property" -> "set_id", "value" -> 1)
      }

      def onUpgrade(db: SQLiteDatabase, oldVer: Int, newVer: Int) {
        db.exec("DROP TABLE IF EXISTS prime;")
        db.exec("DROP TABLE IF EXISTS sets;")
        onCreate(db)
      }
    }
  }

  trait PrefProp[T] {
    def get(db: SQLiteDatabase, section: String, key: String): Option[T] = {
      db.raw("SELECT value FROM prime WHERE section = ? AND key = ?", section, key).get("value")(getter)
    }

    def put(db: SQLiteDatabase, section: String, key: String, value: T) {
      //todo: delete set values from sets table if they exist
      db.replace("prime", "section" -> section, "key" -> key, "value" -> value, "setValue" -> 0)(stringContentValuesPutter, stringContentValuesPutter, putter, intContentValuesPutter)
    }

    def ? (db: SQLiteDatabase, section: String, key: String): Boolean = get(db, section, key).isDefined

    def increment(db: SQLiteDatabase, section: String, key: String): Option[T] = {
      db.transaction { db =>
        db.exec("INSERT OR IGNORE INTO prime (section, key, value) VALUES (?, ?, 1)", section, key)
        db.exec("UPDATE prime SET value = value + 1 WHERE section = ? AND key = ?", section, key)
        db.raw("SELECT value FROM prime WHERE section = ? AND key = ?", section, key).getAndClose("value")(getter)
      } .flatten
    }

    private def getSetId(db: SQLiteDatabase): Option[Long] = {
      db.exec("UPDATE meta SET value = value + 1 WHERE property = ?", "set_id")
      db.raw("SELECT value FROM meta WHERE property = ?", "set_id").getAndClose[Long]("value")
    }

    def setAdd(db: SQLiteDatabase, section: String, key: String, value: T) {
      db.transaction { db =>
        val check = db.raw("SELECT value, setValue FROM prime WHERE section = ? AND key = ?", section, key)
        if (check.isEmpty) { // New set, no previous value
          getSetId(db) map { setId =>
            db.insert("prime", "section" -> section, "key" -> key, "value" -> setId, "setValue" -> 1)
            db.insert("sets", "section" -> section, "setId" -> setId, "value" -> value)(stringContentValuesPutter, longContentValuesPutter, putter)
          }
        }
        else if (check.get[Int]("setValue").exists(_ == 1)) { // Already a set
          check.get[Int]("value") map { prime =>
            db.insert("sets", "section" -> section, "setId" -> prime, "value" -> value)(stringContentValuesPutter, intContentValuesPutter, putter)
          }
        }
        else { // Assuming there is a previous non-set value assigned to this key
          getSetId(db) map { setId =>
            db.replace("prime", "section" -> section, "key" -> key, "value" -> setId, "setValue" -> 1)
            db.insert("sets", "section" -> section, "setId" -> setId, "value" -> value)(stringContentValuesPutter, longContentValuesPutter, putter)
          }
        }
        check.close()
      }
    }

    def setRemove(db: SQLiteDatabase, section: String, key: String, value: T) {
      db.transaction { db =>
        val check = db.raw("SELECT value, setValue FROM prime WHERE section = ? AND key = ?", section, key)
        if (check.isEmpty) { // Nothing there
          //throw KeyNotDefinedException(section, key)
          $w("Key not defined! " + cyborg.util.debug.getStackTrace)
        }
        else if (check.get[Int]("setValue").exists(_ == 0)) { // Value defined, but not a set
          //throw NotASetException(section, key)
          $w("Not a set! " + cyborg.util.debug.getStackTrace)
        }
        else {
          check.get[Long]("value") map { setId =>
            db.delete("sets", "section = ? AND setId = ? AND value = ?", section, setId, value.toString)
          }
        }
        check.close()
      }
    }

    def setGet(db: SQLiteDatabase, section: String, key: String): List[T] = {
      db.transaction { db =>
        val check = db.raw("SELECT value, setValue FROM prime WHERE section = ? AND key = ?", section, key)
        val result = if (check.isEmpty) // Nothing there
          List.empty
        else if (check.get[Int]("setValue").exists(_ == 0)) { // Value defined, but not a set
          //throw NotASetException(section, key)
          $w("Not a set! " + cyborg.util.debug.getStackTrace)
          List.empty
        }
        else {
          check.get[Long]("value") .map { setId =>
            db.raw("SELECT value FROM sets WHERE section = ? AND setId = ?", section, setId.toString)
              .toTypedList[T]("value")(getter)
          } .getOrElse (List.empty)
        }
        check.close()
        result
      } .getOrElse (List.empty)
    }

    def getter: CursorGetter[T]
    def putter: ContentValuesPutter[T]
    def getAndroidPref(prefs: SharedPreferences, key: String): Option[T]
  }

  implicit val stringPrefProp = new PrefProp[String] {
    def getter = stringCursorGetter
    def putter = stringContentValuesPutter
    def getAndroidPref(prefs: SharedPreferences, key: String) = Option(prefs.getString(key, null))
  }

  implicit val intPrefProp = new PrefProp[Int] {
    def getter = intCursorGetter
    def putter = intContentValuesPutter
    def getAndroidPref(prefs: SharedPreferences, key: String) =
      if (prefs.contains(key)) Some(prefs.getInt(key, 0)) else None
  }

  implicit val longPrefProp = new PrefProp[Long] {
    def getter = longCursorGetter
    def putter = longContentValuesPutter
    def getAndroidPref(prefs: SharedPreferences, key: String) =
      if (prefs.contains(key)) Some(prefs.getLong(key, 0)) else None
  }

  implicit val booleanPrefProp = new PrefProp[Boolean] {
    def getter = booleanCursorGetter
    def putter = booleanContentValuesPutter
    override def ? (db: SQLiteDatabase, section: String, key: String): Boolean = {
      get(db, section, key) getOrElse false
    }
    def getAndroidPref(prefs: SharedPreferences, key: String) =
      if (prefs.contains(key)) Some(prefs.getBoolean(key, false)) else None
  }

  class JavaPreferences(androidContext: android.content.Context, section: String, androidPrefsSection: String) {
    assert(androidContext != null)
    assert(section != null)

    implicit val context: Context = androidContext
    val p = new Preferences(section, androidPrefsSection)

    def getString(key: String, default: String): String = p[String](key) getOrElse default
    def getInt(key: String, default: Int): Int = p[Int](key) getOrElse default
    def getLong(key: String, default: Long): Long = p[Long](key) getOrElse default
    def getBoolean(key: String, default: Boolean): Boolean = p[Boolean](key) getOrElse default

    def setString(key: String, value: String) { p(key) = value }
    def setInt(key: String, value: Int) { p(key) = value }
    def setLong(key: String, value: Long) { p(key) = value }
    def setBoolean(key: String, value: Boolean) { p(key) = value }
  }
}