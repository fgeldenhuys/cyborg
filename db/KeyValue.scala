package cyborg.db

import cyborg.Context.Context
import cyborg.db.SQLite._
import android.database.sqlite.{SQLiteDatabaseLockedException, SQLiteDatabase, SQLiteOpenHelper}
import cyborg.Log._
import android.content.ContentValues

class KeyValue(val sqlite: SQLiteDatabase, val bucket: String) {

  def close() { sqlite.close() }

  // Select single value
  private val applyQuery = s"SELECT value FROM '$bucket' WHERE key = ?"
  def apply(key: String): Option[String] = sqlite(applyQuery, key)("value")

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
    for (row <- sqlite(allQuery).toList)
      yield (row("key"), row("value"))

  // Apply once-off filter for result
  private val selectQuery = s"SELECT key, value FROM '$bucket'"
  def select(filter: String => Boolean): List[(String, String)] =
    for (row <- sqlite(selectQuery).toList if (filter(row("key"))))
      yield (row("key"), row("value"))

  // Glob for entries
  private val findQuery = s"SELECT key, value FROM '$bucket' WHERE key GLOB ?"
  def find(glob: String): Seq[(String, String)] =
    for (row <- sqlite(findQuery, glob).toList)
      yield (row("key"), row("value"))

  // Apply once-off filter for result on glob
  private val findAndSelectQuery = s"SELECT key, value FROM '$bucket' WHERE key GLOB ?"
  def findAndSelect(glob: String, filter: String => Boolean): List[(String, String)] = {
    for (row <- sqlite(findAndSelectQuery, glob).toList if (filter(row("key"))))
      yield (row("key"), row("value"))
  }

  class Binary {
    // Select single blob value
    def apply(key: String): Option[Array[Byte]] = sqlite(applyQuery, key).blob("value")

    // Update a single blob value
    def update(key: String, value: Array[Byte]) {
      val contentValues = new ContentValues(2)
      contentValues.put("key", key)
      contentValues.put("value", value)
      sqlite.replace(bucket, null, contentValues)
    }

    def find(glob: String): Seq[(String, Array[Byte])] =
      for (row <- sqlite(findQuery, glob).toBlobList)
        yield (row("key").left.get, row("value").right.get)
  }
  val binary = new Binary
}

object KeyValue {
  val MaxWaitForLockMillis = 1000

  class DbOpenHelper(val bucket: String)(implicit context: Context)
    extends SQLiteOpenHelper(context, "CyborgKeyValueDb", null, 1) {

    def onCreate(db: SQLiteDatabase) {
      db.execSQL(
        s"""
          | CREATE TABLE '$bucket' (
          |   key TEXT PRIMARY KEY NOT NULL,
          |   value BLOB NOT NULL
          | );
        """.stripMargin)
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