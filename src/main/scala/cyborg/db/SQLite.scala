package cyborg.db

import android.database.sqlite.{SQLiteDatabase => ASQLD, SQLiteDatabaseLockedException, SQLiteOpenHelper}
import android.database.{Cursor => AC}
import android.content.ContentValues
import scala.annotation.tailrec

object SQLite {
  type StringOrBlob = Either[String, Array[Byte]]

  case class FailTransaction() extends Exception
  def failTransaction() { throw FailTransaction() }

  trait ContentValuesPutter[T] {
    def apply(cv: ContentValues, key: String, value: T)
  }
  implicit val intContentValuesPutter = new ContentValuesPutter[Int] {
    def apply(cv: ContentValues, key: String, value: Int) {
      cv.put(key, new java.lang.Integer(value))
    }
  }
  implicit val longContentValuesPutter = new ContentValuesPutter[Long] {
    def apply(cv: ContentValues, key: String, value: Long) {
      cv.put(key, new java.lang.Long(value))
    }
  }
  implicit val stringContentValuesPutter = new ContentValuesPutter[String] {
    def apply(cv: ContentValues, key: String, value: String) {
      cv.put(key, value)
    }
  }
  implicit val booleanContentValuesPutter = new ContentValuesPutter[Boolean] {
    def apply(cv: ContentValues, key: String, value: Boolean) {
      cv.put(key, value)
    }
  }
  implicit val byteArrayContentValuesPutter = new ContentValuesPutter[Array[Byte]] {
    def apply(cv: ContentValues, key: String, value: Array[Byte]) {
      cv.put(key, value)
    }
  }

  implicit class Database(val db: ASQLD) extends AnyVal {
    def apply[T](f: (ASQLD) => T): T = {
      val result = f(db)
      db.close()
      result
    }

    def raw(sql: String, args: String*) = db.rawQuery(sql, args.toArray)
    def exec(sql: String, args: String*) = db.execSQL(sql, args.toArray)

    // TODO: maybe make transaction also close db at the end like apply
    def transaction[T](f: (ASQLD) => T): Option[T] = {
      try {
        db.beginTransaction()
        try {
          val result = f(db)
          db.setTransactionSuccessful()
          Some(result)
        }
        catch {
          case ft: FailTransaction =>
            None
        }
      }
      finally {
        db.endTransaction()
      }
    }

    def delete(table: String, whereClause: String, whereArgs: Any*): Int =
      db.delete(table, whereClause, whereArgs.map(_.toString).toArray)

    def makeContentValues[T1](v1: (String, T1))
                             (implicit cvputter1: ContentValuesPutter[T1]): ContentValues = {
      val cv = new ContentValues(2)
      cvputter1(cv, v1._1, v1._2)
      cv
    }

    def makeContentValues[T1, T2](v1: (String, T1), v2: (String, T2))
                                     (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2]): ContentValues = {
      val cv = new ContentValues(2)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cv
    }

    def makeContentValues[T1, T2, T3](v1: (String, T1), v2: (String, T2), v3: (String, T3))
                                     (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3]): ContentValues = {
      val cv = new ContentValues(3)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cv
    }

    def makeContentValues[T1, T2, T3, T4](v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4))
                                     (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4]): ContentValues = {
      val cv = new ContentValues(4)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cvputter4(cv, v4._1, v4._2)
      cv
    }

    def insert[T1, T2](table: String, v1: (String, T1), v2: (String, T2))
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2]): Option[Long] = {
      val result = db.insert(table, null, makeContentValues(v1, v2))
      if (result == -1) None else Some(result)
    }

    def insert[T1, T2, T3](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3))
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3]): Option[Long] = {
      val result = db.insert(table, null, makeContentValues(v1, v2, v3))
      if (result == -1) None else Some(result)
    }

    def insert[T1, T2, T3, T4](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4))
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4]): Option[Long] = {
      val result = db.insert(table, null, makeContentValues(v1, v2, v3, v4))
      if (result == -1) None else Some(result)
    }

    def replace[T1, T2](table: String, v1: (String, T1), v2: (String, T2))
                   (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2]): Option[Long] = {
      val result = db.replace(table, null, makeContentValues(v1, v2))
      if (result == -1) None else Some(result)
    }

    def replace[T1, T2, T3](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3))
                       (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3]): Option[Long] = {
      val result = db.replace(table, null, makeContentValues(v1, v2, v3))
      if (result == -1) None else Some(result)
    }

    def replace[T1, T2, T3, T4](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4))
                           (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4]): Option[Long] = {
      val result = db.replace(table, null, makeContentValues(v1, v2, v3, v4))
      if (result == -1) None else Some(result)
    }

    def update[T1](table: String, v1: (String, T1),
                       whereClause: String, whereArgs: Any*)
                      (implicit cvputter1: ContentValuesPutter[T1]): Int =
      db.update(table, makeContentValues(v1), whereClause, whereArgs.map(_.toString).toArray)

    def update[T1, T2](table: String, v1: (String, T1), v2: (String, T2),
                           whereClause: String, whereArgs: Any*)
                      (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2]): Int =
      db.update(table, makeContentValues(v1, v2), whereClause, whereArgs.map(_.toString).toArray)

    def update[T1, T2, T3](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3),
                           whereClause: String, whereArgs: Any*)
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3]): Int =
      db.update(table, makeContentValues(v1, v2, v3), whereClause, whereArgs.map(_.toString).toArray)

  }

  implicit class OpenHelper(val oh: SQLiteOpenHelper) extends AnyVal {
    import cyborg.Log._

    // TODO: the cancel timeout thing is broken
    // def readableDatabase(tryUntil: Long = 0): ASQLD = {
    //  val cancel = if (tryUntil == 0) System.currentTimeMillis() + 1000 else tryUntil
    def readableDatabase: ASQLD = {
      val cancel = System.currentTimeMillis() + 1000
      try {
        oh.getReadableDatabase
      }
      catch {
        case e: SQLiteDatabaseLockedException =>
          if (System.currentTimeMillis() > cancel) throw e
          $w("Waiting for locked database: " + e.getMessage)
          try { Thread.sleep(10) } catch { case e: InterruptedException => }
          readableDatabase
      }
    }

    def writableDatabase: ASQLD = {
      val cancel = System.currentTimeMillis() + 1000
      try {
        oh.getWritableDatabase
      }
      catch {
        case e: SQLiteDatabaseLockedException =>
          if (System.currentTimeMillis() > cancel) throw e
          $w("Waiting for locked database: " + e.getMessage)
          try { Thread.sleep(10) } catch { case e: InterruptedException => }
          writableDatabase
      }
    }
  }

  trait CursorGetter[T] {
    def get(cursor: AC, column: Int): Option[T]
  }
  implicit val stringCursorGetter = new CursorGetter[String] {
    def get(cursor: AC, column: Int): Option[String] = Option(cursor.getString(column))
  }
  implicit val blobCursorGetter = new CursorGetter[Array[Byte]] {
    def get(cursor: AC, column: Int): Option[Array[Byte]] = Option(cursor.getBlob(column))
  }
  implicit val intCursorGetter = new CursorGetter[Int] {
    def get(cursor: AC, column: Int): Option[Int] = Option(cursor.getInt(column))
  }
  implicit val longCursorGetter = new CursorGetter[Long] {
    def get(cursor: AC, column: Int): Option[Long] = Option(cursor.getLong(column))
  }
  implicit val booleanCursorGetter = new CursorGetter[Boolean] {
    def get(cursor: AC, column: Int): Option[Boolean] = Option(cursor.getInt(column) == 1)
  }

  implicit class CursorExt(val cursor: AC) /* extends AnyVal */ { // @tailrec is broken with AnyVal
    def apply[T](f: (AC) => T): T = {
      val result = f(cursor)
      cursor.close()
      result
    }

    def get[T](columnName: String)(implicit getter: CursorGetter[T]): Option[T] = {
      if (cursor.getCount > 0) {
        if (cursor.isBeforeFirst) cursor.moveToNext()
        getter.get(cursor, cursor.getColumnIndex(columnName))
      }
      else None
    }

    def getAndClose[T](columnName: String)(implicit getter: CursorGetter[T]): Option[T] = {
      val result: Option[T] = get[T](columnName)
      cursor.close()
      result
    }

    def isEmpty: Boolean = cursor.getCount == 0

    def toList: List[Map[String, String]] = {
      cursor.moveToPosition(-1)
      val result = toListHelper()
      cursor.close()
      result
    }

    def toList(field: String): List[String] = {
      cursor.moveToPosition(-1)
      val result = toListHelperWithField(field)
      cursor.close()
      result
    }

    def toBlobList: List[Map[String, StringOrBlob]] = {
      cursor.moveToPosition(-1)
      val result = toBlobListHelper()
      cursor.close()
      result
    }

    def toTypedList[T](field: String)(implicit getter: CursorGetter[T]): List[T] = {
      cursor.moveToPosition(-1)
      val result = toTypedListHelper[T](field)
      cursor.close()
      result
    }

    private def cursor2map(cursor: AC): Map[String, String] =
      cursor.getColumnNames.map(name =>
        (name, cursor.getString(cursor.getColumnIndex(name)))).toMap

    private def cursor2blobMap(cursor: AC): Map[String, StringOrBlob] = {
      cursor.getColumnNames.map { name =>
        val index = cursor.getColumnIndex(name)
        if (cursor.getType(index) == AC.FIELD_TYPE_BLOB)
          (name, Right(cursor.getBlob(index)))
        else
          (name, Left(cursor.getString(index)))
      }.toMap
    }

    @tailrec private def toListHelper(acc: List[Map[String, String]] = List.empty): List[Map[String, String]] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()

      if (cursor.isAfterLast) acc
      else toListHelper(cursor2map(cursor) :: acc)
    }

    @tailrec private def toListHelperWithField(field: String, acc: List[String] = List.empty): List[String] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()

      if (cursor.isAfterLast) acc
      else toListHelperWithField(field, cursor.getString(cursor.getColumnIndex(field)) +: acc)
    }

    @tailrec private def toBlobListHelper(acc: List[Map[String, StringOrBlob]] = List.empty): List[Map[String, StringOrBlob]] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()

      if (cursor.isAfterLast) acc
      else toBlobListHelper(cursor2blobMap(cursor) :: acc)
    }

    @tailrec private def toTypedListHelper[T](field: String, acc: List[T] = List.empty[T])(implicit getter: CursorGetter[T]): List[T] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()

      if (cursor.isAfterLast) acc
      else toTypedListHelper[T](field, getter.get(cursor, cursor.getColumnIndex(field)).map(_ +: acc) getOrElse acc)
    }

    // Use when SELECT COUNT(*) type query was used
    def countQueryResult: Int = {
      cursor.moveToFirst()
      if (cursor.getCount > 0 && cursor.getColumnCount > 0)
        cursor.getInt(0)
      else 0
    }
  }
}
