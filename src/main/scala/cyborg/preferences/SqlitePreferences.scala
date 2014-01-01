package cyborg.preferences

import cyborg.Context._
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import cyborg.db.SQLite._

object SqlitePreferences {
  case class KeyNotDefinedException(section: String, key: String) extends Exception(s"Key not set '$key' for section '$section'")
  case class NotASetException(section: String, key: String) extends Exception(s"Key '$key' is not a set in section '$section'")

  class Preferences(val section: String)(implicit val context: Context) {
    val helper = new DbOpenHelper

    def apply[T](key: String)(implicit prop: PrefProp[T]): Option[T] =
      helper.readableDatabase(db => prop.get(db, section, key))

    def update[T](key: String, value: T)(implicit prop: PrefProp[T]) {
      helper.writableDatabase(db => prop.put(db, section, key, value))
    }

    def ? [T](key: String)(implicit prop: PrefProp[T]): Boolean = {
      helper.readableDatabase(db => prop.?(db, section, key))
    }

    def delete(key: String) {
      helper.writableDatabase { db =>
        db.delete("prime", "section = ? AND key = ?", section, key)
      }
    }

    def setOrDelete[T](key: String, value: Option[T])(implicit prop: PrefProp[T]) {
      value map (v => update[T](key, v)) getOrElse delete(key)
    }

    def makeDefault[T](key: String, value: T)(implicit prop: PrefProp[T]) {
      if (apply[T](key).isEmpty) update[T](key, value)
    }

    class PrefSet(val key: String) {
      def += [T](value: T)(implicit prop: PrefProp[T]) {
        helper.writableDatabase(db => prop.setAdd(db, section, key, value))
      }

      def -= [T](value: T)(implicit prop: PrefProp[T]) {
        helper.writableDatabase(db => prop.setRemove(db, section, key, value))
      }

      def delete() {
        helper.writableDatabase.transaction { db =>
          val check = db.raw("SELECT value, set FROM prime WHERE section = ? AND key = ?", section, key)
          if (check.isEmpty) // Nothing there
            throw KeyNotDefinedException(section, key)
          else if (check.get[Int]("set").exists(_ == 0)) // Value defined, but not a set
            throw NotASetException(section, key)
          else {
            check.get[Long]("value") map { setId =>
              db.delete("sets", "section = ? AND setId = ?", section, setId)
              db.delete("prime", "section = ? AND key = ?", section, key)
            }
          }
        }
      }
    }

    def set(key: String) = new PrefSet(key)

    class DbOpenHelper extends SQLiteOpenHelper(context, "CyborgPreferencesDb", null, 1) {
      override def onOpen(db: SQLiteDatabase) {
        db.raw(
          """
            | CREATE TABLE IF NOT EXISTS prime (
            |   id INTEGER PRIMARY KEY NOT NULL,
            |   section TEXT NOT NULL,
            |   key TEXT NOT NULL,
            |   value BLOB NOT NULL,
            |   set INTEGER DEFAULT 0,
            |   UNIQUE(section, key) ON CONFLICT REPLACE
            | );
          """.stripMargin)
        db.raw(
          """
            | CREATE TABLE IF NOT EXISTS meta (
            |   property TEXT PRIMARY KEY,
            |   value BLOB NOT NULL
            | );
          """.stripMargin)
        db.raw(
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

      def onCreate(db: SQLiteDatabase) {}

      def onUpgrade(db: SQLiteDatabase, oldVer: Int, newVer: Int) {
        db.raw("DROP TABLE IF EXISTS prime;")
        db.raw("DROP TABLE IF EXISTS sets;")
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
      db.replace("prime", "section" -> section, "key" -> key, "value" -> value, "set" -> 0)(stringContentValuesPutter, stringContentValuesPutter, putter, intContentValuesPutter)
    }

    def ? (db: SQLiteDatabase, section: String, key: String): Boolean = get(db, section, key).isDefined

    private def getSetId(db: SQLiteDatabase): Option[Long] = {
      db.raw("UPDATE meta SET value = value + 1 WHERE property = ?", "set_id")
      db.raw("SELECT value FROM meta WHERE property = ?", "set_id").get[Long]("value")
    }

    def setAdd(db: SQLiteDatabase, section: String, key: String, value: T) {
      db.transaction { db =>
        val check = db.raw("SELECT value, set FROM prime WHERE section = ? AND key = ?", section, key)
        if (check.isEmpty) { // New set, no previous value
          getSetId(db) map { setId =>
            db.insert("prime", "section" -> section, "key" -> key, "value" -> setId, "set" -> 1)
            db.insert("sets", "section" -> section, "setId" -> setId, "value" -> value)(stringContentValuesPutter, longContentValuesPutter, putter)
          }
        }
        else if (check.get[Int]("set").exists(_ == 1)) { // Already a set
          check.get[Int]("value") map { prime =>
            db.insert("sets", "section" -> section, "setId" -> prime, "value" -> value)(stringContentValuesPutter, intContentValuesPutter, putter)
          }
        }
        else { // Assuming there is a previous non-set value assigned to this key
          getSetId(db) map { setId =>
            db.replace("prime", "section" -> section, "key" -> key, "value" -> setId, "set" -> 1)
            db.insert("sets", "section" -> section, "setId" -> setId, "value" -> value)(stringContentValuesPutter, longContentValuesPutter, putter)
          }
        }
      }
    }

    def setRemove(db: SQLiteDatabase, section: String, key: String, value: T) {
      db.transaction { db =>
        val check = db.raw("SELECT value, set FROM prime WHERE section = ? AND key = ?", section, key)
        if (check.isEmpty) // Nothing there
          throw KeyNotDefinedException(section, key)
        else if (check.get[Int]("set").exists(_ == 0)) // Value defined, but not a set
          throw NotASetException(section, key)
        else {
          check.get[Long]("value") map { setId =>
            db.delete("sets", "section = ? AND setId = ? AND value = ?", section, setId, value.toString)
          }
        }
      }
    }

    def getter: CursorGetter[T]
    def putter: ContentValuesPutter[T]
  }

  implicit val stringPrefProp = new PrefProp[String] {
    def getter = stringCursorGetter
    def putter = stringContentValuesPutter
  }

  implicit val intPrefProp = new PrefProp[Int] {
    def getter = intCursorGetter
    def putter = intContentValuesPutter
  }

  implicit val booleanPrefProp = new PrefProp[Boolean] {
    def getter = booleanCursorGetter
    def putter = booleanContentValuesPutter
    override def ? (db: SQLiteDatabase, section: String, key: String): Boolean = {
      get(db, section, key) getOrElse false
    }
  }

}