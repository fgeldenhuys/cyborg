package cyborg.db

import android.database.sqlite.{SQLiteDatabase => ASQLD, SQLiteOpenHelper}
import android.database.{Cursor => AC}
import android.content.ContentValues
import cyborg.Context.Context
import scala.annotation.tailrec
import scala.concurrent.duration._
import cyborg.util.control._
import cyborg.util.execution._
import cyborg.Log._
import scalaz._, Scalaz._
import java.util.concurrent.locks.ReentrantReadWriteLock

object SQLite {
  type StringOrBlob = Either[String, Array[Byte]]
  val TooLongOperation = 1000l
  val DefaultCutoff = 1000l

  case class FailTransaction() extends Exception
  def failTransaction() { throw FailTransaction() }

  object ContentValuesHelper {
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

    def apply[T1](v1: (String, T1))
                             (implicit cvputter1: ContentValuesPutter[T1]): ContentValues = {
      val cv = new ContentValues(2)
      cvputter1(cv, v1._1, v1._2)
      cv
    }

    def apply[T1, T2](v1: (String, T1), v2: (String, T2))
                                 (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2]): ContentValues = {
      val cv = new ContentValues(2)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cv
    }

    def apply[T1, T2, T3](v1: (String, T1), v2: (String, T2), v3: (String, T3))
                                     (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3]): ContentValues = {
      val cv = new ContentValues(3)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cv
    }

    def apply[T1, T2, T3, T4](v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4))
                                         (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4]): ContentValues = {
      val cv = new ContentValues(4)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cvputter4(cv, v4._1, v4._2)
      cv
    }
  }

  implicit class Database(val db: ASQLD) extends AnyVal {
    import ContentValuesHelper._

    /*def apply[T](f: (ASQLD) => T): T = {
      val before = systemTime
      val result = f(db)
      val dtime = systemTime - before
      db.close()
      if (dtime > TooLongOperation) {
        $w(s"WARNING Database operation took $dtime milliseconds")
        cyborg.util.debug.printStackTrace()
      }
      result
    }*/

    def raw[A](sql: String, args: String*)(f: AC => A) = {
      val cursor = db.rawQuery(sql, args.toArray)
      assert(db.isOpen) // TODO: take out this assert
      val result = f(cursor)
      cursor.close()
      result
    }

    def exec(sql: String, args: String*) = db.execSQL(sql, args.toArray)

    def transaction[T](f: (ASQLD) => T): Throwable \/ T = {
      val before = systemTime
      try {
        db.beginTransaction()
        try {
          val result = f(db)
          db.setTransactionSuccessful()
          \/-(result)
        }
        catch {
          case ft: FailTransaction =>
            -\/(ft)
          case e: Throwable =>
            $e("ERROR during transaction: " + e.toString)
            e.printStackTrace()
            -\/(e)
        }
      }
      finally {
        val dtime = systemTime - before
        db.endTransaction()
        if (dtime > TooLongOperation) {
          $w(s"WARNING Database transaction took $dtime milliseconds")
          cyborg.util.debug.printStackTrace()
        }
      }
    }

    def delete(table: String, whereClause: String, whereArgs: Any*): Int =
      db.delete(table, whereClause, whereArgs.map(_.toString).toArray)

    def insert[T1, T2](table: String, v1: (String, T1), v2: (String, T2))
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2]): Option[Long] = {
      val result = db.insert(table, null, ContentValuesHelper(v1, v2))
      if (result == -1) None else Some(result)
    }

    def insert[T1, T2, T3](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3))
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3]): Option[Long] = {
      val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3))
      if (result == -1) None else Some(result)
    }

    def insert[T1, T2, T3, T4](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4))
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4]): Option[Long] = {
      val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3, v4))
      if (result == -1) None else Some(result)
    }

    def replace[T1, T2](table: String, v1: (String, T1), v2: (String, T2))
                   (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2]): Option[Long] = {
      val result = db.replace(table, null, ContentValuesHelper(v1, v2))
      if (result == -1) None else Some(result)
    }

    def replace[T1, T2, T3](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3))
                       (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3]): Option[Long] = {
      val result = db.replace(table, null, ContentValuesHelper(v1, v2, v3))
      if (result == -1) None else Some(result)
    }

    def replace[T1, T2, T3, T4](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4))
                           (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4]): Option[Long] = {
      val result = db.replace(table, null, ContentValuesHelper(v1, v2, v3, v4))
      if (result == -1) None else Some(result)
    }

    def update[T1](table: String, v1: (String, T1),
                       whereClause: String, whereArgs: Any*)
                      (implicit cvputter1: ContentValuesPutter[T1]): Int =
      db.update(table, ContentValuesHelper(v1), whereClause, whereArgs.map(_.toString).toArray)

    def update[T1, T2](table: String, v1: (String, T1), v2: (String, T2),
                           whereClause: String, whereArgs: Any*)
                      (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2]): Int =
      db.update(table, ContentValuesHelper(v1, v2), whereClause, whereArgs.map(_.toString).toArray)

    def update[T1, T2, T3](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3),
                           whereClause: String, whereArgs: Any*)
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3]): Int =
      db.update(table, ContentValuesHelper(v1, v2, v3), whereClause, whereArgs.map(_.toString).toArray)

  }

  abstract class OpenHelper(name: String,
                            version: Int,
                            val retryTime: Duration = 1.second,
                            val pauseTime: Duration = 25.millis)
                           (implicit context: Context)
    extends SQLiteOpenHelper(context, name, null, version) {

    import cyborg.Log._

    val lock = new ReentrantReadWriteLock()

    def read[A](f: ASQLD => A): Throwable \/ A = {
      lock.w.retryFor(retryTime, pauseTime) { // Changing this to read lock causes DB locks, seems like even reads need to be exclusive
        val db = getReadableDatabase
        assert(db.isOpen)
        if (db.isDbLockedByCurrentThread) $w("DB is locked by current thread!")
        else if(db.isDbLockedByOtherThreads) $w("DB is locked by other threads!")
        val result = f(db)
        db.close()
        result
      }
    }

    def write[A](f: ASQLD => A): Throwable \/ A = {
      lock.w.retryFor(retryTime, pauseTime) {
        val db = getWritableDatabase
        assert(db.isOpen)
        if (db.isDbLockedByCurrentThread) $w("DB is locked by current thread!")
        else if(db.isDbLockedByOtherThreads) $w("DB is locked by other threads!")
        val result = f(db)
        db.close()
        result
      }
    }

    def writeTransaction[A](f: ASQLD => A): Throwable \/ A = {
      write[Throwable \/ A] { db =>
        db.transaction[A] { tdb =>
          f(tdb)
        }
      } .flatMap(identity)
    }
  }

  trait CursorGetter[T] {
    def get(cursor: AC, column: Int): T
  }
  implicit val stringCursorGetter = new CursorGetter[String] {
    def get(cursor: AC, column: Int) = cursor.getString(column)
  }
  implicit val blobCursorGetter = new CursorGetter[Array[Byte]] {
    def get(cursor: AC, column: Int) = cursor.getBlob(column)
  }
  implicit val intCursorGetter = new CursorGetter[Int] {
    def get(cursor: AC, column: Int) = cursor.getInt(column)
  }
  implicit val longCursorGetter = new CursorGetter[Long] {
    def get(cursor: AC, column: Int) = cursor.getLong(column)
  }
  implicit val booleanCursorGetter = new CursorGetter[Boolean] {
    def get(cursor: AC, column: Int) = cursor.getInt(column) == 1
  }

  class CursorColumnIterator[T](val cursor: AC, c1: Int)
                               (implicit val getter: CursorGetter[T])
                               extends Iterator[T] {
    if (cursor.isBeforeFirst) cursor.moveToFirst()
    override def hasNext: Boolean = !cursor.isLast
    override def next(): T = {
      cursor.moveToNext()
      getter.get(cursor, c1)
    }
  }

  class Cursor2ColumnIterator[T1, T2](val cursor: AC, c1: Int, c2: Int)
                                     (implicit val getter1: CursorGetter[T1], val getter2: CursorGetter[T2])
                                     extends Iterator[(T1, T2)] {
    if (cursor.isBeforeFirst) cursor.moveToFirst()
    override def hasNext: Boolean = !cursor.isLast
    override def next(): (T1, T2) = {
      cursor.moveToNext()
      (getter1.get(cursor, c1), getter2.get(cursor, c2))
    }
  }

  implicit class CursorExt(val cursor: AC) /* extends AnyVal */ { // @tailrec is broken with AnyVal
    def get[T](columnName: String)(implicit getter: CursorGetter[T]): Option[T] = {
      tryOption {
        if (cursor.getCount > 0) {
          if (cursor.isBeforeFirst) cursor.moveToNext()
          Some(getter.get(cursor, cursor.getColumnIndex(columnName)))
        }
        else None
      } .flatten
    }

    def isEmpty: Boolean = cursor.getCount == 0

    def toColumnIterator[A](c1: String)(implicit getter: CursorGetter[A]): CursorColumnIterator[A] =
      new CursorColumnIterator[A](cursor, cursor.getColumnIndex(c1))

    def toColumnIterator[A1, A2](c1: String, c2: String)
                                (implicit getter1: CursorGetter[A1], getter2: CursorGetter[A2]):
                                Cursor2ColumnIterator[A1, A2] =
      new Cursor2ColumnIterator[A1, A2](cursor, cursor.getColumnIndex(c1), cursor.getColumnIndex(c2))

    def toList: List[Map[String, String]] = {
      cursor.moveToPosition(-1)
      val result = toListHelper()
      //cursor.close()
      result
    }

    def toList(field: String): List[String] = {
      cursor.moveToPosition(-1)
      val result = toListHelperWithField(field)
      //cursor.close()
      result
    }

    def toBlobList: List[Map[String, StringOrBlob]] = {
      cursor.moveToPosition(-1)
      val result = toBlobListHelper()
      //cursor.close()
      result
    }

    def toTypedList[T](field: String)(implicit getter: CursorGetter[T]): List[T] = {
      cursor.moveToPosition(-1)
      val result = toTypedListHelper[T](field)
      //cursor.close()
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
      else toTypedListHelper[T](field, getter.get(cursor, cursor.getColumnIndex(field)) +: acc)
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
