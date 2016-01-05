package cyborg.preferences

import android.content.SharedPreferences
import android.database.Cursor
import android.database.sqlite.SQLiteDatabase
import cyborg.Context, cyborg.Context._
import cyborg.crypto.CryptoKey
import cyborg.db.SQLite._
import cyborg.Log._
import cyborg.util.control._
import cyborg.util.execution.ScheduledExecutionContext
import scalaz._, Scalaz._

object SqlitePreferences {
  import ContentValuesHelper._

  case class KeyNotDefinedException(section: String, key: String) extends Exception(s"Key not set '$key' for section '$section'")
  case class NotASetException(section: String, key: String) extends Exception(s"Key '$key' is not a set in section '$section'")

  class Preferences(val section: String,
                    val cryptoKey: CryptoKey,
                    val androidPrefsSection: String,
                    sqliteFailingCallback: Option[Throwable => Unit])
                   (implicit val context: Context) {
    private def helper = DbOpenHelper(sqliteFailingCallback)

    lazy val androidPrefs: Option[SharedPreferences] = {
      assert(context != null)
      assert(context.c != null)
      assert(androidPrefsSection != null)
      tryOption(context.getSharedPreferences(androidPrefsSection, Context.ModeMultiProcess))
    }

    def apply[T](key: String)(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Option[T] = {
      helper.read[Option[T]](db => prop.get(db, section, key)).toOption.flatten
        .orElse(androidPrefs.flatMap(ap => prop.getAndroidPref(ap, key)))
    }

    def update[T](key: String, value: T)(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Unit = {
      helper.write(db => prop.put(db, section, key, value))
    }

    def put[T](pairs: List[(String, T)])(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Unit = {
      helper.write(db => prop.putMulti(db, section, pairs))
    }

    def ? (key: String)(implicit prop: PrefProp[Boolean], sec: ScheduledExecutionContext): Boolean = {
      helper.read(db => prop.?(db, section, key)).getOrElse(false)
    }

    def delete(key: String)(implicit sec: ScheduledExecutionContext): Unit = {
      helper.write(_.delete("prime", "section = ? AND key = ?", section, key))
    }

    def delete(keys: List[String])(implicit sec: ScheduledExecutionContext): Unit = {
      helper.writeTransaction { db =>
        keys map { key => db.delete("prime", "section = ? AND key = ?", section, key) }
      }
    }

    def exists(key: String)(implicit sec: ScheduledExecutionContext): Boolean = {
      helper.read { db =>
        db.raw("SELECT EXISTS(SELECT 1 FROM prime WHERE section = ? AND key = ? LIMIT 1)", section, key) { c =>
          c.moveToFirst()
          c.getInt(0) == 1
        }
      } getOrElse false
    }

    def glob[T](query: String)(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): List[T] = {
      helper.read(db => prop.glob(db, section, query)).getOrElse(List.empty)
    }

    def globDelete(glob: String)(implicit sec: ScheduledExecutionContext): Unit = {
      helper.write(_.delete("prime", "section = ? AND key GLOB ?", section, glob))
    }

    def setOrDelete[T](key: String, value: Option[T])(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Unit = {
      value map (v => update[T](key, v)) getOrElse delete(key)
    }

    def makeDefault[T](key: String, value: T)(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Unit = {
      if (apply[T](key).isEmpty) update[T](key, value)
    }

    def increment[T](key: String)(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Option[T] = {
      helper.write(db => prop.increment(db, section, key)).toOption.flatten
    }

    def all[T](implicit prop: PrefProp[T], sec: ScheduledExecutionContext): List[(String, T)] = {
      helper.read(db => prop.all(db, section)).toOption.getOrElse(List.empty)
    }

    class PrefSet(val key: String) {
      def += [T](value: T)(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Unit = {
        helper.write(db => prop.setAdd(db, section, key, value))
      }

      def replaceWith[T](list: List[T])(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Unit = {
        helper.write(db => prop.setReplace(db, section, key, list))
      }

      def -= [T](value: T)(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Unit = {
        helper.write(db => prop.setRemove(db, section, key, value))
      }

      def delete()(implicit sec: ScheduledExecutionContext): Unit = {
        helper.writeTransaction { db =>
          db.raw("SELECT value, setValue FROM prime WHERE section = ? AND key = ?", section, key) { check =>
            if (check.isEmpty) { // Nothing there
              //throw KeyNotDefinedException(section, key)
              $w(s"Key '$key' not defined in section '$section'!")
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
          }
        }
      }

      def contains[T](value: T)(implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Boolean = {
        helper.read(db => prop.setContains(db, section, key, value)) getOrElse false
      }

      def toList[T](implicit prop: PrefProp[T], sec: ScheduledExecutionContext): List[T] = {
        helper.read(db => prop.setGet(db, section, key)).getOrElse(List.empty)
      }
      def toSet[T](implicit prop: PrefProp[T], sec: ScheduledExecutionContext): Set[T] = toList[T](prop, sec).toSet
    }
    def set(key: String) = new PrefSet(key)

    class PrefSecure {
      import cyborg.util.binary._
      import cyborg.crypto.SimpleEncryption._

      def apply[T](key: String)(implicit encryption: SimpleEncryption[T], sec: ScheduledExecutionContext): Option[T] = {
        (helper read { db =>
          for (str <- stringPrefProp.get(db, section, key)) yield decrypt[T](str.decodeBase64, cryptoKey)
        }).toOption.flatten
      }

      def update[T](key: String, value: T)(implicit encryption: SimpleEncryption[T], sec: ScheduledExecutionContext): Unit = {
        val data = encrypt[T](value, cryptoKey).base64
        helper.write(db => stringPrefProp.put(db, section, key, data))
      }
    }
    def secure = new PrefSecure
  }

  class DbOpenHelper private (sqliteFailingCallback: Option[Throwable => Unit])
                             (implicit context: Context) extends OpenHelper("CyborgPreferencesDb", 1, sqliteFailingCallback) {
    def onCreate(db: SQLiteDatabase): Unit = {
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

    def onUpgrade(db: SQLiteDatabase, oldVer: Int, newVer: Int): Unit = {
      db.exec("DROP TABLE IF EXISTS prime;")
      db.exec("DROP TABLE IF EXISTS sets;")
      onCreate(db)
    }
  }

  object DbOpenHelper {
    private var instance: Option[DbOpenHelper] = None

    def apply(sqliteFailingCallback: Option[Throwable => Unit])(implicit context: Context): DbOpenHelper = {
      if (instance.isEmpty) {
        $d("*** CREATING PREFERENCES DB OPEN HELPER INSTANCE ***")
        instance = Option(new DbOpenHelper(sqliteFailingCallback))
      }
      instance.get
    }
  }

  trait PrefProp[T] {
    def get(db: SQLiteDatabase, section: String, key: String): Option[T] = {
      db.raw("SELECT value FROM prime WHERE section = ? AND key = ?", section, key)(_.get("value")(getter))
    }

    def put(db: SQLiteDatabase, section: String, key: String, value: T): Unit = {
      //todo: delete set values from sets table if they exist
      db.replace("prime", "section" -> section, "key" -> key, "value" -> value, "setValue" -> 0)(stringContentValuesPutter, stringContentValuesPutter, putter, intContentValuesPutter)
      ()
    }

    def putMulti(db: SQLiteDatabase, section: String, pairs: List[(String, T)]): Unit = {
      db.transaction { db =>
        for ((key, value) <- pairs) {
          db.replace("prime", "section" -> section, "key" -> key, "value" -> value, "setValue" -> 0)(stringContentValuesPutter, stringContentValuesPutter, putter, intContentValuesPutter)
        }
      }.leftMap(t => $w("DB transaction failed: " + t.getStackTraceString))
      ()
    }

    def ? (db: SQLiteDatabase, section: String, key: String): Boolean = get(db, section, key).isDefined

    def glob(db: SQLiteDatabase, section: String, query: String): List[T] = {
      db.raw("SELECT value FROM prime WHERE section = ? AND key GLOB ?", section, query)(_.toTypedList("value")(getter))
    }

    def increment(db: SQLiteDatabase, section: String, key: String): Option[T] = {
      db.transaction { db =>
        db.exec("INSERT OR IGNORE INTO prime (section, key, value) VALUES (?, ?, 1)", section, key)
        db.exec("UPDATE prime SET value = value + 1 WHERE section = ? AND key = ?", section, key)
        db.raw("SELECT value FROM prime WHERE section = ? AND key = ?", section, key)(_.get("value")(getter))
      } .toOption.flatten
    }

    def all(db: SQLiteDatabase, section: String): List[(String, T)] = {
      db.raw("SELECT key, value FROM prime WHERE section = ?", section)(_.toColumnIterator[String, T]("key", "value")(stringCursorGetter, getter).toList)
    }

    private def getSetId(db: SQLiteDatabase): Option[Long] = {
      db.exec("UPDATE meta SET value = value + 1 WHERE property = ?", "set_id")
      db.raw("SELECT value FROM meta WHERE property = ?", "set_id")(_.get[Long]("value"))
    }

    def setAdd(db: SQLiteDatabase, section: String, key: String, value: T): Unit = {
      db.transaction { db =>
        db.raw("SELECT value, setValue FROM prime WHERE section = ? AND key = ?", section, key) { check =>
          if (check.isEmpty) { // New set, no previous value
            getSetId(db) map { setId =>
              db.insert("prime", "section" -> section, "key" -> key, "value" -> setId, "setValue" -> 1)
              db.insert("sets", "section" -> section, "setId" -> setId, "value" -> value)(stringContentValuesPutter, longContentValuesPutter, putter)
            }
          }
          else if (check.get[Int]("setValue").exists(_ == 1)) { // Already a set
            check.get[Long]("value") map { prime =>
              db.insert("sets", "section" -> section, "setId" -> prime, "value" -> value)(stringContentValuesPutter, longContentValuesPutter, putter)
            }
          }
          else { // Assuming there is a previous non-set value assigned to this key
            getSetId(db) map { setId =>
              db.replace("prime", "section" -> section, "key" -> key, "value" -> setId, "setValue" -> 1)
              db.insert("sets", "section" -> section, "setId" -> setId, "value" -> value)(stringContentValuesPutter, longContentValuesPutter, putter)
            }
          }
        }
      }.leftMap(t => $w("DB transaction failed: " + t.getStackTraceString))
      ()
    }

    def setReplace(db: SQLiteDatabase, section: String, key: String, values: List[T]): Unit = {
      db.transaction { db =>
        db.raw("SELECT value, setValue FROM prime WHERE section = ? AND key = ?", section, key) { check =>
          val optSetId = if (check.isEmpty) { // New set, no previous value
            getSetId(db) map { setId =>
              db.insert("prime", "section" -> section, "key" -> key, "value" -> setId, "setValue" -> 1)
              setId
            }
          }
          else if (check.get[Int]("setValue").exists(_ == 1)) { // Already a set
            check.get[Long]("value")
          }
          else { // Assuming there is a previous non-set value assigned to this key
            getSetId(db) map { setId =>
              db.replace("prime", "section" -> section, "key" -> key, "value" -> setId, "setValue" -> 1)
              setId
            }
          }
          optSetId map { setId =>
            db.delete("sets", "section = ? AND setId = ?", section, setId)
            values foreach { value =>
              db.insert("sets", "section" -> section, "setId" -> setId, "value" -> value)(stringContentValuesPutter, longContentValuesPutter, putter)
            }
          } getOrElse {
            $e(s"CONFIG failed to get a set id for '$key'")
          }
        }
      }.leftMap(t => $w("DB transaction failed: " + t.getStackTraceString))
      ()
    }

    def setRemove(db: SQLiteDatabase, section: String, key: String, value: T): Unit = {
      db.transaction { db =>
        db.raw("SELECT value, setValue FROM prime WHERE section = ? AND key = ?", section, key) { check =>
          if (check.isEmpty) { // Nothing there
            //throw KeyNotDefinedException(section, key)
            $w(s"Trying to remove a value from an undefined set: '$key'")
          }
          else if (check.get[Int]("setValue").exists(_ == 0)) { // Value defined, but not a set
            //throw NotASetException(section, key)
            $w(s"Trying to remove a value from something other than a set: '$key'\n" + cyborg.util.debug.getStackTrace)
          }
          else {
            check.get[Long]("value") map { setId =>
              db.delete("sets", "section = ? AND setId = ? AND value = ?", section, setId, value.toString)
            }
          }
        }
      }.leftMap(t => $w("DB transaction failed: " + t.getStackTraceString))
      ()
    }

    def setQuery[A](db: SQLiteDatabase, section: String, key: String, errval: A)(f: Cursor => A): A = {
      db.transaction { db =>
        db.raw("SELECT value, setValue FROM prime WHERE section = ? AND key = ?", section, key) { check =>
          if (check.isEmpty) // Nothing there
            errval
          else if (check.get[Int]("setValue").exists(_ == 0)) { // Value defined, but not a set
            //throw NotASetException(section, key)
            $w("Not a set! " + cyborg.util.debug.getStackTrace)
            errval
          }
          else {
            check.get[Long]("value") .map { setId =>
              db.raw("SELECT value FROM sets WHERE section = ? AND setId = ?", section, setId.toString) { cursor =>
                f(cursor)
              }
            } .getOrElse (errval)
          }
        }
      } .getOrElse (errval)
    }

    def setGet(db: SQLiteDatabase, section: String, key: String): List[T] = {
      setQuery[List[T]](db, section, key, List.empty[T]) { cursor =>
        cursor.toTypedList[T]("value")(getter)
      }
    }

    def setContains(db: SQLiteDatabase, section: String, key: String, value: T): Boolean = {
      setQuery[Boolean](db, section, key, false) { cursor =>
        cursor.toColumnIterator("value")(getter) exists (_ == value)
      }
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

  class JavaPreferences(androidContext: android.content.Context,
                        section: String,
                        cryptoKey: CryptoKey,
                        androidPrefsSection: String,
                        scheduledExecutionContext: ScheduledExecutionContext) {
    assert(androidContext != null)
    assert(section != null)

    implicit val context: Context = androidContext
    implicit val sec = scheduledExecutionContext

    val p = new Preferences(section, cryptoKey, androidPrefsSection, None)

    def getString(key: String, default: String): String = p[String](key) getOrElse default
    def getInt(key: String, default: Int): Int = p[Int](key) getOrElse default
    def getLong(key: String, default: Long): Long = p[Long](key) getOrElse default
    def getBoolean(key: String, default: Boolean): Boolean = p[Boolean](key) getOrElse default

    def getSecureString(key: String, default: String): String = p.secure[String](key) getOrElse default

    def setString(key: String, value: String): Unit = { p(key) = value }
    def setInt(key: String, value: Int): Unit = { p(key) = value }
    def setLong(key: String, value: Long): Unit = { p(key) = value }
    def setBoolean(key: String, value: Boolean): Unit = { p(key) = value }

    def setSecureString(key: String, value: String): Unit = { p.secure(key) = value }
  }
}