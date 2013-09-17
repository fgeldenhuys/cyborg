package cyborg.db

import cyborg.Context._
import cyborg.db.SQLite._
import android.database.sqlite.{SQLiteDatabaseLockedException, SQLiteDatabase, SQLiteOpenHelper}
import cyborg.Log._
import android.content.ContentValues

class KeyValue(val sqlite: SQLiteDatabase, val bucket: String) {

  def close() { sqlite.close() }

  // Select single value
  private val applyQuery = s"SELECT value FROM '$bucket' WHERE key = ?"
  def apply(key: String): Option[String] = sqlite.raw(applyQuery, key).get[String]("value")

  // Update a single value
  def update(key: String, value: String) {
    val contentValues = new ContentValues(2)
    contentValues.put("key", key)
    contentValues.put("value", value)
    sqlite.replace(bucket, null, contentValues)
  }

  // Delete an entry
  def del(key: String) {
    sqlite.delete(bucket, "key = ?", Array(key))
  }

  // All entries
  private val allQuery = s"SELECT key, value FROM '$bucket'"
  def all: Seq[(String, String)] =
    for (row <- sqlite.raw(allQuery).toList)
      yield (row("key"), row("value"))

  // Apply once-off filter for result
  private val selectQuery = s"SELECT key, value FROM '$bucket'"
  def select(filter: String => Boolean): List[(String, String)] =
    for (row <- sqlite.raw(selectQuery).toList if filter(row("key")))
      yield (row("key"), row("value"))

  // Glob for entries
  private val findQuery = s"SELECT key, value FROM '$bucket' WHERE key GLOB ?"
  def find(glob: String): Seq[(String, String)] =
    for (row <- sqlite.raw(findQuery, glob).toList)
      yield (row("key"), row("value"))

  // Apply once-off filter for result on glob
  private val findAndSelectQuery = s"SELECT key, value FROM '$bucket' WHERE key GLOB ?"
  def findAndSelect(glob: String, filter: String => Boolean): List[(String, String)] = {
    for (row <- sqlite.raw(findAndSelectQuery, glob).toList if filter(row("key")))
      yield (row("key"), row("value"))
  }

  class Binary {
    // Select single blob value
    def apply(key: String): Option[Array[Byte]] = sqlite.raw(applyQuery, key).get[Array[Byte]]("value")

    // Update a single blob value
    def update(key: String, value: Array[Byte]) {
      val contentValues = new ContentValues(2)
      contentValues.put("key", key)
      contentValues.put("value", value)
      sqlite.replace(bucket, null, contentValues)
    }

    def find(glob: String): Seq[(String, Array[Byte])] =
      for (row <- sqlite.raw(findQuery, glob).toBlobList)
        yield (row("key").left.get, row("value").right.get)
  }
  val binary = new Binary

  class StringIndex {
    val stringIndexStringQuery = s"SELECT stringindex FROM '$bucket' WHERE key = ?"
    def string(key: String): Option[String] =
      sqlite.raw(stringIndexStringQuery, key).get[String]("stringindex")

    val stringIndexKeysQuery = s"SELECT key FROM '$bucket' WHERE stringindex LIKE ?"
    def keys(find: String): List[String] =
      sqlite.raw(stringIndexKeysQuery, s"%$find%").toList("key")

    val stringIndexQuery = s"SELECT value FROM '$bucket' WHERE stringindex LIKE ?"
    def apply(find: String): List[String] =
      sqlite.raw(stringIndexQuery, s"%$find%").toList("value")


    def update(key: String, string: String) {
      val contentValues = new ContentValues(2)
      contentValues.put("key", key)
      contentValues.put("stringindex", string)
      sqlite.replace(bucket, null, contentValues)
    }
  }
  val stringIndex = new StringIndex
}

object KeyValue {
  val MaxWaitForLockMillis = 1000

  //TODO: create bucket table if it doesn't exist, and remove version parameter
  class DbOpenHelper(val bucket: String, version: Int = 1)(implicit context: Context)
    extends SQLiteOpenHelper(context, "CyborgKeyValueDb", null, version) {

    override def onOpen(db: SQLiteDatabase) {
      db.execSQL(
        s"""
          | CREATE TABLE IF NOT EXISTS '$bucket' (
          |   key TEXT PRIMARY KEY NOT NULL,
          |   stringindex TEXT DEFAULT NULL,
          |   value BLOB NOT NULL
          | );
        """.stripMargin)
    }

    def onCreate(db: SQLiteDatabase) {
    }

    def onUpgrade(db: SQLiteDatabase, oldVer: Int, newVer: Int) {
      db.execSQL(s"DROP TABLE IF EXISTS '$bucket';")
      onCreate(db)
    }

    def readable: KeyValue = {
      val cancel = System.currentTimeMillis() + MaxWaitForLockMillis
      try {
        new KeyValue(getReadableDatabase, bucket)
      }
      catch {
        case e: SQLiteDatabaseLockedException =>
          if (System.currentTimeMillis() > cancel) throw e
          $i("Waiting for locked database: " + e.getMessage)
          try { Thread.sleep(10) } catch { case e: InterruptedException => }
          readable
      }
    }

    def writable: KeyValue = {
      val cancel = System.currentTimeMillis() + MaxWaitForLockMillis
      try {
        new KeyValue(getWritableDatabase, bucket)
      }
      catch {
        case e: SQLiteDatabaseLockedException =>
          if (System.currentTimeMillis() > cancel) throw e
          $i("Waiting for locked database: " + e.getMessage)
          try { Thread.sleep(10) } catch { case e: InterruptedException => }
          writable
      }
    }
  }

}