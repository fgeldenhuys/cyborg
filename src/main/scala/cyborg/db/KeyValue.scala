package cyborg.db

import android.database.sqlite.{SQLiteDatabaseLockedException, SQLiteDatabase, SQLiteOpenHelper}
import cyborg.Context._
import cyborg.db.SQLite._
import cyborg.Log._
import cyborg.util.io._
import cyborg.util.control._
import java.io._

class KeyValue(val sqlite: SQLiteDatabase, val bucket: String) {
  import cyborg.db.KeyValue.KeyValueException

  def close() { stackTraceHandler(Unit) { sqlite.close() } }
  def transaction[T](f: (KeyValue) => T): Option[T] = sqlite.transaction(f(this))

  private val applyQuery = s"SELECT value FROM '$bucket' WHERE key = ?"
  private val allQuery = s"SELECT key, value FROM '$bucket'"
  private val countQuery = s"SELECT COUNT(*) FROM '$bucket' WHERE key GLOB ?"
  private val exportQuery = s"SELECT key, stringindex, value FROM '$bucket'"

  val ExportIdentifier = "cyborg.db.KeyValue"
  val ExportVersion: Int = 1
  val StringIdentifier: Byte = 0x11
  val BlobIdentifier: Byte = 0x12

  // Select single value
  def apply(key: String): Option[String] = sqlite.raw(applyQuery, key).get[String]("value")

  // Update a single value
  def update(key: String, value: String) {
    val result = sqlite.replace(bucket, "key" -> key, "value" -> value)
    if (result.isEmpty) throw KeyValueException(s"binary.update($key) failed")
  }

  // Delete an entry
  def del(key: String) {
    sqlite.delete(bucket, "key = ?", key)
  }

  // Glob delete
  def findAndDel(glob: String) {
    sqlite.delete(bucket, "key GLOB ?", glob)
  }

  // All entries
  def all: Seq[(String, String)] =
    for (row <- sqlite.raw(allQuery).toList)
      yield (row("key"), row("value"))

  // Apply once-off filter for result
  def select(filter: String => Boolean): List[(String, String)] =
    for (row <- sqlite.raw(allQuery).toList if filter(row("key")))
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

  def count(glob: String): Int =
    sqlite.raw(countQuery, glob).countQueryResult


  class Binary {
    // Select single blob value
    def apply(key: String): Option[Array[Byte]] = sqlite.raw(applyQuery, key).get[Array[Byte]]("value")

    // Update a single blob value
    def update(key: String, value: Array[Byte]) {
      val result = sqlite.replace(bucket, "key" -> key, "value" -> value)
      if (result.isEmpty) throw KeyValueException(s"binary.update($key) failed")
    }

    def find(glob: String): Seq[(String, Array[Byte])] =
      for (row <- sqlite.raw(findQuery, glob).toBlobList)
        yield (row("key").left.get, row("value").right.get)
  }
  val binary = new Binary

  class StringIndex {
    val stringIndexStringQuery = s"SELECT stringindex FROM '$bucket' WHERE key = ?"
    val stringIndexKeysQuery = s"SELECT key FROM '$bucket' WHERE stringindex LIKE ?"
    val stringIndexQuery = s"SELECT value FROM '$bucket' WHERE stringindex LIKE ?"

    def string(key: String): Option[String] =
      sqlite.raw(stringIndexStringQuery, key).get[String]("stringindex")

    def keys(find: String): List[String] =
      sqlite.raw(stringIndexKeysQuery, s"%$find%").toList("key")

    def apply(find: String): List[String] =
      sqlite.raw(stringIndexQuery, s"%$find%").toList("value")

    def update(key: String, string: String) {
      sqlite.update(bucket, "stringindex" -> string, "key = ?", key)
    }
  }
  val stringIndex = new StringIndex

  def exportToOutputStream(out: OutputStream) {
    val data = sqlite.raw(exportQuery).toBlobList
    out << ExportIdentifier << ExportVersion << data.size
    for (row <- data) {
      val key = row("key").left.get
      val stringindex = Option(row("stringindex").left.get)
      val value = row("value")
      out encString key
      stringindex match {
        case Some(string) => out encString string
        case None => out << 0
      }
      value match {
        case Left(string) =>
          out << StringIdentifier encString string
        case Right(blob) =>
          out << BlobIdentifier << blob.size << blob
      }
    }
  }

  def exportToFile(file: File) {
    val out = new BufferedOutputStream(new FileOutputStream(file))
    exportToOutputStream(out)
    out.close()
  }

  case class ImportException(message: String) extends Exception(message)

  def importFromInputStream(in: InputStream) {
    if (in.string(ExportIdentifier.size) != ExportIdentifier) throw ImportException("Export identifier mismatch")
    if (in.int != ExportVersion) throw ImportException("Export version unknown")
    val entries = in.int
    sqlite.transaction {
      for (entry <- 0 until entries) {
        val key = in.decString
        val stringindex = in.decString
        val valueType = in.byte
        valueType match {
          case StringIdentifier =>
            val string = in.decString
            if (stringindex.isEmpty)
              sqlite.insert(bucket, "key" -> key, "value" -> string)
            else
              sqlite.insert(bucket, "key" -> key, "stringindex" -> stringindex, "value" -> string)
          case BlobIdentifier =>
            val size = in.int
            val blob = in.bytes(size)
            if (stringindex.isEmpty)
              sqlite.insert(bucket, "key" -> key, "value" -> blob)
            else
              sqlite.insert(bucket, "key" -> key, "stringindex" -> stringindex, "value" -> blob)
          case _ =>
            throw ImportException("Unknown value type byte encountered")
        }
      }
    }
  }

  def importFromFile(file: File) {
    val in = new BufferedInputStream(new FileInputStream(file))
    importFromInputStream(in)
    in.close()
  }
}

object KeyValue {
  val MaxWaitForLockMillis = 1000

  case class KeyValueException(message: String) extends Exception(message) {
    $e(cyborg.util.debug.getStackTrace)
  }

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