package cyborg.db

import java.util.concurrent.Semaphore

import android.database.sqlite.{SQLiteDatabase => ASQLD, SQLiteOpenHelper}
import android.database.{Cursor => AC}
import android.content.ContentValues

import scala.annotation.tailrec
import scala.concurrent.duration._
import scalaz._, Scalaz._, scalaz.concurrent._

import cyborg.Context.Context
import cyborg.util.execution._
import cyborg.util.task._
import cyborg.util.scalazext._
import cyborg.Log._

object SQLite {
  type StringOrBlob = Either[String, Array[Byte]]
  val TooLongOperation = 1000l
  val DefaultCutoff = 1000l

  case class FailTransaction() extends Exception
  def failTransaction(): Unit = { throw FailTransaction() }
  case class InvalidColumnException(name: String) extends Exception(s"Invalid column name '$name'")

  sealed trait WhereArgs {
    def selection: String
    def args: Array[String]
  }
  case class SomeWhereArgs(selection: String, args: Array[String]) extends WhereArgs
  case object NoWhereArgs extends WhereArgs {
    override def selection = null
    override def args = Array.empty[String]
  }
  object Where {
    def apply(where: String, args: String*) = SomeWhereArgs(where, args.toArray)
  }

  object ContentValuesHelper {
    trait ContentValuesPutter[T] {
      def apply(cv: ContentValues, key: String, value: T): Unit
    }

    implicit val intContentValuesPutter = new ContentValuesPutter[Int] {
      def apply(cv: ContentValues, key: String, value: Int): Unit = {
        cv.put(key, new java.lang.Integer(value))
      }
    }
    implicit val longContentValuesPutter = new ContentValuesPutter[Long] {
      def apply(cv: ContentValues, key: String, value: Long): Unit = {
        cv.put(key, new java.lang.Long(value))
      }
    }
    implicit val stringContentValuesPutter = new ContentValuesPutter[String] {
      def apply(cv: ContentValues, key: String, value: String): Unit = {
        cv.put(key, value)
      }
    }
    implicit val booleanContentValuesPutter = new ContentValuesPutter[Boolean] {
      def apply(cv: ContentValues, key: String, value: Boolean): Unit = {
        cv.put(key, value)
      }
    }
    implicit val byteArrayContentValuesPutter = new ContentValuesPutter[Array[Byte]] {
      def apply(cv: ContentValues, key: String, value: Array[Byte]): Unit = {
        cv.put(key, value)
      }
    }
    implicit val floatContentValuesPutter = new ContentValuesPutter[Float] {
      def apply(cv: ContentValues, key: String, value: Float): Unit = {
        cv.put(key, new java.lang.Float(value))
      }
    }

    implicit val intOptionContentValuesPutter = new ContentValuesPutter[Option[Int]] {
      def apply(cv: ContentValues, key: String, value: Option[Int]): Unit = {
        value map (x => cv.put(key, new java.lang.Integer(x))) getOrElse cv.putNull(key)
      }
    }
    implicit val longOptionContentValuesPutter = new ContentValuesPutter[Option[Long]] {
      def apply(cv: ContentValues, key: String, value: Option[Long]): Unit = {
        value map (x => cv.put(key, new java.lang.Long(x))) getOrElse cv.putNull(key)
      }
    }
    implicit val stringOptionContentValuesPutter = new ContentValuesPutter[Option[String]] {
      def apply(cv: ContentValues, key: String, value: Option[String]): Unit = {
        value map (x => cv.put(key, x)) getOrElse cv.putNull(key)
      }
    }
    implicit val booleanOptionContentValuesPutter = new ContentValuesPutter[Option[Boolean]] {
      def apply(cv: ContentValues, key: String, value: Option[Boolean]): Unit = {
        value map (x => cv.put(key, x)) getOrElse cv.putNull(key)
      }
    }
    implicit val byteArrayOptionContentValuesPutter = new ContentValuesPutter[Option[Array[Byte]]] {
      def apply(cv: ContentValues, key: String, value: Option[Array[Byte]]): Unit = {
        value map (x => cv.put(key, x)) getOrElse cv.putNull(key)
      }
    }
    implicit val floatOptionContentValuesPutter = new ContentValuesPutter[Option[Float]] {
      def apply(cv: ContentValues, key: String, value: Option[Float]): Unit = {
        value map (x => cv.put(key, new java.lang.Float(x))) getOrElse cv.putNull(key)
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

    def apply[T1, T2, T3, T4, T5](v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5))
                             (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5]): ContentValues = {
      val cv = new ContentValues(5)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cvputter4(cv, v4._1, v4._2)
      cvputter5(cv, v5._1, v5._2)
      cv
    }

    def apply[T1, T2, T3, T4, T5, T6](v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6))
                                 (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6]): ContentValues = {
      val cv = new ContentValues(6)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cvputter4(cv, v4._1, v4._2)
      cvputter5(cv, v5._1, v5._2)
      cvputter6(cv, v6._1, v6._2)
      cv
    }

    def apply[T1, T2, T3, T4, T5, T6, T7](v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7))
                                 (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7]): ContentValues = {
      val cv = new ContentValues(7)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cvputter4(cv, v4._1, v4._2)
      cvputter5(cv, v5._1, v5._2)
      cvputter6(cv, v6._1, v6._2)
      cvputter7(cv, v7._1, v7._2)
      cv
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8](v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7), v8: (String, T8))
             (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7], cvputter8: ContentValuesPutter[T8]): ContentValues = {
      val cv = new ContentValues(8)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cvputter4(cv, v4._1, v4._2)
      cvputter5(cv, v5._1, v5._2)
      cvputter6(cv, v6._1, v6._2)
      cvputter7(cv, v7._1, v7._2)
      cvputter8(cv, v8._1, v8._2)
      cv
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9](v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7), v8: (String, T8), v9: (String, T9))
                                 (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7], cvputter8: ContentValuesPutter[T8], cvputter9: ContentValuesPutter[T9]): ContentValues = {
      val cv = new ContentValues(9)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cvputter4(cv, v4._1, v4._2)
      cvputter5(cv, v5._1, v5._2)
      cvputter6(cv, v6._1, v6._2)
      cvputter7(cv, v7._1, v7._2)
      cvputter8(cv, v8._1, v8._2)
      cvputter9(cv, v9._1, v9._2)
      cv
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7), v8: (String, T8), v9: (String, T9), v10: (String, T10))
                                 (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7], cvputter8: ContentValuesPutter[T8], cvputter9: ContentValuesPutter[T9], cvputter10: ContentValuesPutter[T10]): ContentValues = {
      val cv = new ContentValues(10)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cvputter4(cv, v4._1, v4._2)
      cvputter5(cv, v5._1, v5._2)
      cvputter6(cv, v6._1, v6._2)
      cvputter7(cv, v7._1, v7._2)
      cvputter8(cv, v8._1, v8._2)
      cvputter9(cv, v9._1, v9._2)
      cvputter10(cv, v10._1, v10._2)
      cv
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7), v8: (String, T8), v9: (String, T9), v10: (String, T10), v11: (String, T11))
                                 (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7], cvputter8: ContentValuesPutter[T8], cvputter9: ContentValuesPutter[T9], cvputter10: ContentValuesPutter[T10], cvputter11: ContentValuesPutter[T11]): ContentValues = {
      val cv = new ContentValues(11)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cvputter4(cv, v4._1, v4._2)
      cvputter5(cv, v5._1, v5._2)
      cvputter6(cv, v6._1, v6._2)
      cvputter7(cv, v7._1, v7._2)
      cvputter8(cv, v8._1, v8._2)
      cvputter9(cv, v9._1, v9._2)
      cvputter10(cv, v10._1, v10._2)
      cvputter11(cv, v11._1, v11._2)
      cv
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7), v8: (String, T8), v9: (String, T9), v10: (String, T10), v11: (String, T11), v12: (String, T12))
                                 (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7], cvputter8: ContentValuesPutter[T8], cvputter9: ContentValuesPutter[T9], cvputter10: ContentValuesPutter[T10], cvputter11: ContentValuesPutter[T11], cvputter12: ContentValuesPutter[T12]): ContentValues = {
      val cv = new ContentValues(12)
      cvputter1(cv, v1._1, v1._2)
      cvputter2(cv, v2._1, v2._2)
      cvputter3(cv, v3._1, v3._2)
      cvputter4(cv, v4._1, v4._2)
      cvputter5(cv, v5._1, v5._2)
      cvputter6(cv, v6._1, v6._2)
      cvputter7(cv, v7._1, v7._2)
      cvputter8(cv, v8._1, v8._2)
      cvputter9(cv, v9._1, v9._2)
      cvputter10(cv, v10._1, v10._2)
      cvputter11(cv, v11._1, v11._2)
      cvputter12(cv, v12._1, v12._2)
      cv
    }
  }

  implicit class Database(val db: ASQLD) extends AnyVal {
    import ContentValuesHelper._

    def raw[A](sql: String, args: String*)(f: AC => A) = {
      val cursor = db.rawQuery(sql, args.toArray)
      try { f(cursor) }
      finally { cursor.close() }
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

    def select[T1](table: String, f1: String, where: WhereArgs)
                  (implicit get1: CursorGetter[T1]): List[T1] = {
      val cursor = db.query(table, Array(f1), where.selection, where.args, null, null, null)
      try { cursor.toTypedList[T1](f1) }
      finally { cursor.close() }
    }

    def select[T1, T2](table: String, f1: String, f2: String, where: WhereArgs)
                      (implicit get1: CursorGetter[T1], get2: CursorGetter[T2]): List[(T1, T2)] = {
      val cursor = db.query(table, Array(f1, f2), where.selection, where.args, null, null, null)
      try { cursor.toTypedList[T1, T2](f1, f2) }
      finally { cursor.close() }
    }

    def select[T1, T2, T3](table: String, f1: String, f2: String, f3: String, where: WhereArgs)
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3]): List[(T1, T2, T3)] = {
      val cursor = db.query(table, Array(f1, f2, f3), where.selection, where.args, null, null, null)
      try { cursor.toTypedList[T1, T2, T3](f1, f2, f3) }
      finally { cursor.close() }
    }

    def select[T1, T2, T3, T4](table: String, f1: String, f2: String, f3: String, f4: String, where: WhereArgs)
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4]): List[(T1, T2, T3, T4)] = {
      val cursor = db.query(table, Array(f1, f2, f3, f4), where.selection, where.args, null, null, null)
      try { cursor.toTypedList[T1, T2, T3, T4](f1, f2, f3, f4) }
      finally { cursor.close() }
    }

    def select[T1, T2, T3, T4, T5](table: String, f1: String, f2: String, f3: String, f4: String, f5: String, where: WhereArgs)
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5]): List[(T1, T2, T3, T4, T5)] = {
      val cursor = db.query(table, Array(f1, f2, f3, f4, f5), where.selection, where.args, null, null, null)
      try { cursor.toTypedList[T1, T2, T3, T4, T5](f1, f2, f3, f4, f5) }
      finally { cursor.close() }
    }

    def select[T1, T2, T3, T4, T5, T6](table: String, f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, where: WhereArgs)
                                      (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6]): List[(T1, T2, T3, T4, T5, T6)] = {
      val cursor = db.query(table, Array(f1, f2, f3, f4, f5, f6), where.selection, where.args, null, null, null)
      try { cursor.toTypedList[T1, T2, T3, T4, T5, T6](f1, f2, f3, f4, f5, f6) }
      finally { cursor.close() }
    }

    def select[T1, T2, T3, T4, T5, T6, T7](table: String, f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, where: WhereArgs)
                                      (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7]): List[(T1, T2, T3, T4, T5, T6, T7)] = {
      val cursor = db.query(table, Array(f1, f2, f3, f4, f5, f6, f7), where.selection, where.args, null, null, null)
      try { cursor.toTypedList[T1, T2, T3, T4, T5, T6, T7](f1, f2, f3, f4, f5, f6, f7) }
      finally { cursor.close() }
    }

    def delete(table: String, whereClause: String, whereArgs: Any*): Int =
      db.delete(table, whereClause, whereArgs.map(_.toString).toArray)

    def deleteAll(table: String): Int =
      db.delete(table, "1")

    def insert[T1, T2](table: String, v1: (String, T1), v2: (String, T2))
                      (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insert(table, null, ContentValuesHelper(v1, v2))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
    }

    def insert[T1, T2, T3](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3))
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
    }

    def insert[T1, T2, T3, T4](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4))
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3, v4))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
    }

    def insert[T1, T2, T3, T4, T5](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5))
                              (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3, v4, v5))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
    }

    def insert[T1, T2, T3, T4, T5, T6](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6))
                              (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3, v4, v5, v6))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
    }

    def insert[T1, T2, T3, T4, T5, T6, T7](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7))
                              (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3, v4, v5, v6, v7))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
    }

    def insert[T1, T2, T3, T4, T5, T6, T7, T8](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7), v8: (String, T8))
                              (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7], cvputter8: ContentValuesPutter[T8]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insertOrThrow(table, null, ContentValuesHelper(v1, v2, v3, v4, v5, v6, v7, v8))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
    }

    def insert[T1, T2, T3, T4, T5, T6, T7, T8, T9](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7), v8: (String, T8), v9: (String, T9))
        (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7], cvputter8: ContentValuesPutter[T8], cvputter9: ContentValuesPutter[T9]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3, v4, v5, v6, v7, v8, v9))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
    }

    def insert[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7), v8: (String, T8), v9: (String, T9), v10: (String, T10))
                              (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7], cvputter8: ContentValuesPutter[T8], cvputter9: ContentValuesPutter[T9], cvputter10: ContentValuesPutter[T10]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
    }

    def insert[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7), v8: (String, T8), v9: (String, T9), v10: (String, T10), v11: (String, T11))
                              (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7], cvputter8: ContentValuesPutter[T8], cvputter9: ContentValuesPutter[T9], cvputter10: ContentValuesPutter[T10], cvputter11: ContentValuesPutter[T11]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
    }

    def insert[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6), v7: (String, T7), v8: (String, T8), v9: (String, T9), v10: (String, T10), v11: (String, T11), v12: (String, T12))
                              (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6], cvputter7: ContentValuesPutter[T7], cvputter8: ContentValuesPutter[T8], cvputter9: ContentValuesPutter[T9], cvputter10: ContentValuesPutter[T10], cvputter11: ContentValuesPutter[T11], cvputter12: ContentValuesPutter[T12]): Throwable \/ Long = {
      \/.fromTryCatchNonFatal {
        val result = db.insert(table, null, ContentValuesHelper(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12))
        if (result == -1) throw new Exception("Unknown error")
        result
      }
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

    def replace[T1, T2, T3, T4, T5](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5))
                               (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5]): Option[Long] = {
      val result = db.replace(table, null, ContentValuesHelper(v1, v2, v3, v4, v5))
      if (result == -1) None else Some(result)
    }

    def replace[T1, T2, T3, T4, T5, T6](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5), v6: (String, T6))
                                   (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5], cvputter6: ContentValuesPutter[T6]): Option[Long] = {
      val result = db.replace(table, null, ContentValuesHelper(v1, v2, v3, v4, v5, v6))
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

    def update[T1, T2, T3, T4](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4),
                                   whereClause: String, whereArgs: Any*)
                                  (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4]): Int =
      db.update(table, ContentValuesHelper(v1, v2, v3, v4), whereClause, whereArgs.map(_.toString).toArray)

    def update[T1, T2, T3, T4, T5](table: String, v1: (String, T1), v2: (String, T2), v3: (String, T3), v4: (String, T4), v5: (String, T5),
                           whereClause: String, whereArgs: Any*)
                          (implicit cvputter1: ContentValuesPutter[T1], cvputter2: ContentValuesPutter[T2], cvputter3: ContentValuesPutter[T3], cvputter4: ContentValuesPutter[T4], cvputter5: ContentValuesPutter[T5]): Int =
      db.update(table, ContentValuesHelper(v1, v2, v3, v4, v5), whereClause, whereArgs.map(_.toString).toArray)


    def rawDebugQuery(sql: String, args: String*): String = {
      val c = db.rawQuery(sql, args.toArray)
      try {
        val names = c.getColumnNames
        names.mkString("\t") +
          c.toList.foldLeft("") { (a, x) =>
            a + (names.map { name =>
              x.get(name).getOrElse("-")
            }).mkString("\n", "\t", "")
          }
      }
      finally { c.close() }
    }
  }

  def leakSqliteCursors(dbName: String)(implicit context: Context, sec: ScheduledExecutionContext): Unit = {
    import scalaz._, concurrent._
    var count = 0
    Task.create {
      $d("TEST LEAKING sqlite cursors on db " + dbName)
      val helper = new SQLiteOpenHelper(context, dbName, null, 1) {
        override def onUpgrade(db: ASQLD, oldVersion: Int, newVersion: Int): Unit = {}
        override def onCreate(db: ASQLD): Unit = {}
      }
      val db = helper.getReadableDatabase
      while (true) {
        db.rawQuery("SELECT * FROM sqlite_master", null).moveToFirst()
        count += 1
        if (count % 50 == 0) cyborg.Log.$d(s"TEST LEAKED $count")
        Thread.sleep(1)
      }
    } .runAsync {
      case -\/(t) => $d(s"TEST LEAK killed at $count leaks: " + t)
      case \/-(_) => $d("TEST LEAK this should never happen")
    }
  }

  abstract class OpenHelper(name: String,
                            version: Int,
                            sqliteFailingCallback: Option[Throwable => Unit],
                            val retryTime: Duration = 4.seconds,
                            val pauseTime: Duration = 205.millis)
                           (implicit context: Context)
    extends SQLiteOpenHelper(context, name, null, version) {

    import cyborg.Log._

    val lock = new Semaphore(1, true)

    override def onOpen(db: ASQLD): Unit = {
      db.setForeignKeyConstraintsEnabled(true)
    }

    def read[A](f: ASQLD => A)(implicit sec: ScheduledExecutionContext): Throwable \/ A = {
      Task.create {
        val db = getReadableDatabase
        try {
          assert(db.isOpen)
          if (db.isDbLockedByCurrentThread) $w("DB is locked by current thread!")
          f(db)
        }
        finally {
          db.close()
        }
      } .timed(4000)
        .exclusive(lock, 5000)
        .attemptRun
    }

    def write[A](f: ASQLD => A)(implicit sec: ScheduledExecutionContext): Throwable \/ A = {
      Task.create {
        val db = getWritableDatabase
        try {
          assert(db.isOpen)
          if (db.isDbLockedByCurrentThread) $w("DB is locked by current thread!")
          f(db)
        }
        finally {
          db.close()
        }
      } .timed(4000)
        .exclusive(lock, 5000)
        .attemptRun
    }

    def readTransaction[A](f: ASQLD => A)(implicit sec: ScheduledExecutionContext): Throwable \/ A = {
      read[Throwable \/ A] { db =>
        db.transaction[A] { tdb =>
          f(tdb)
        }
      } .flatMap(identity)
    }

    def writeTransaction[A](f: ASQLD => A)(implicit sec: ScheduledExecutionContext): Throwable \/ A = {
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
  implicit val floatCursorGetter = new CursorGetter[Float] {
    def get(cursor: AC, column: Int) = cursor.getFloat(column)
  }

  implicit val stringOptionCursorGetter = new CursorGetter[Option[String]] {
    def get(cursor: AC, column: Int) = \/.fromTryCatchNonFatal(cursor.getString(column)).toOptionLog.flatMap(Option(_))
  }
  implicit val blobOptionCursorGetter = new CursorGetter[Option[Array[Byte]]] {
    def get(cursor: AC, column: Int) = \/.fromTryCatchNonFatal(cursor.getBlob(column)).toOptionLog.flatMap(Option(_))
  }
  implicit val intOptionCursorGetter = new CursorGetter[Option[Int]] {
    def get(cursor: AC, column: Int) = \/.fromTryCatchNonFatal(cursor.getInt(column)).toOptionLog.flatMap(Option(_))
  }
  implicit val longOptionCursorGetter = new CursorGetter[Option[Long]] {
    def get(cursor: AC, column: Int) = \/.fromTryCatchNonFatal(cursor.getLong(column)).toOptionLog.flatMap(Option(_))
  }
  implicit val booleanOptionCursorGetter = new CursorGetter[Option[Boolean]] {
    def get(cursor: AC, column: Int) = \/.fromTryCatchNonFatal(cursor.getInt(column) == 1).toOptionLog.flatMap(Option(_))
  }
  implicit val floatOptionCursorGetter = new CursorGetter[Option[Float]] {
    def get(cursor: AC, column: Int) = \/.fromTryCatchNonFatal(cursor.getFloat(column)).toOptionLog.flatMap(Option(_))
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

  implicit class CursorExt(val cursor: AC) extends AnyVal { // @tailrec is broken with AnyVal
    def get[T](columnName: String)(implicit getter: CursorGetter[T]): Option[T] = {
      \/.fromTryCatchNonFatal {
        if (cursor.getCount > 0) {
          if (cursor.isBeforeFirst) cursor.moveToNext()
          Some(getter.get(cursor, cursor.getColumnIndex(columnName)))
        }
        else None
      } .toOptionLog.flatten
    }

    def isEmpty: Boolean = cursor.getCount == 0

    def columnIndex(name: String): Int = {
      cursor.getColumnIndex(name) match {
        case index if index >= 0 => index
        case _ => throw InvalidColumnException(name)
      }
    }

    def toColumnIterator[A](c1: String)(implicit getter: CursorGetter[A]): CursorColumnIterator[A] =
      new CursorColumnIterator[A](cursor, cursor.columnIndex(c1))

    def toColumnIterator[A1, A2](c1: String, c2: String)
                                (implicit getter1: CursorGetter[A1], getter2: CursorGetter[A2]):
                                Cursor2ColumnIterator[A1, A2] =
      new Cursor2ColumnIterator[A1, A2](cursor, cursor.columnIndex(c1), cursor.columnIndex(c2))

    def toList: List[Map[String, String]] = {
      cursor.moveToPosition(-1)
      val result = toListHelper()
      result.reverse
    }

    def toList(field: String): List[String] = {
      cursor.moveToPosition(-1)
      val result = toListHelperWithField(field)
      result.reverse
    }

    def toBlobList: List[Map[String, StringOrBlob]] = {
      cursor.moveToPosition(-1)
      val result = toBlobListHelper()
      result.reverse
    }

    def toTypedList[T](field: String)(implicit getter: CursorGetter[T]): List[T] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T](field, List.empty).reverse
    }

    def toTypedList[T1, T2](f1: String, f2: String)
                           (implicit get1: CursorGetter[T1], get2: CursorGetter[T2])
                            :List[(T1, T2)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2](f1, f2, List.empty).reverse
    }

    def toTypedList[T1, T2, T3](f1: String, f2: String, f3: String)
                               (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3])
                                :List[(T1, T2, T3)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3](f1, f2, f3, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4](f1: String, f2: String, f3: String, f4: String)
                               (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4])
                                :List[(T1, T2, T3, T4)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4](f1, f2, f3, f4, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5](f1: String, f2: String, f3: String, f4: String, f5: String)
                                       (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5])
                                        :List[(T1, T2, T3, T4, T5)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5](f1, f2, f3, f4, f5, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String)
                                           (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6])
                                            :List[(T1, T2, T3, T4, T5, T6)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6](f1, f2, f3, f4, f5, f6, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String)
                                                   (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7])
                                                   :List[(T1, T2, T3, T4, T5, T6, T7)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7](f1, f2, f3, f4, f5, f6, f7, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7, T8](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String)
                                                   (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8])
                                                   :List[(T1, T2, T3, T4, T5, T6, T7, T8)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8](f1, f2, f3, f4, f5, f6, f7, f8, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7, T8, T9](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String)
                                                       (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9])
                                                       :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9](f1, f2, f3, f4, f5, f6, f7, f8, f9, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String)
                                                            (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10])
                                                            :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String)
                                                            (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11])
                                                            :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String)
                                                                      (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12])
                                                                      :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String)
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12], get13: CursorGetter[T13])
        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String, f14: String)
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12], get13: CursorGetter[T13], get14: CursorGetter[T14])
        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String, f14: String, f15: String)
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12], get13: CursorGetter[T13], get14: CursorGetter[T14], get15: CursorGetter[T15])
        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String, f14: String, f15: String, f16: String)
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12], get13: CursorGetter[T13], get14: CursorGetter[T14], get15: CursorGetter[T15], get16: CursorGetter[T16])
        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
        f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, List.empty).reverse
    }

    def toTypedList[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String, f14: String, f15: String, f16: String, f17: String, f18: String)
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12], get13: CursorGetter[T13], get14: CursorGetter[T14], get15: CursorGetter[T15], get16: CursorGetter[T16], get17: CursorGetter[T17], get18: CursorGetter[T18])
        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = {
      cursor.moveToPosition(-1)
      toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
        f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, List.empty).reverse
    }

    private def cursor2map(cursor: AC): Map[String, String] =
      cursor.getColumnNames.map(name =>
        (name, cursor.getString(cursor.columnIndex(name)))).toMap

    private def cursor2blobMap(cursor: AC): Map[String, StringOrBlob] = {
      cursor.getColumnNames.map { name =>
        val index = cursor.columnIndex(name)
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
      else toListHelperWithField(field, cursor.getString(cursor.columnIndex(field)) +: acc)
    }

    @tailrec private def toBlobListHelper(acc: List[Map[String, StringOrBlob]] = List.empty): List[Map[String, StringOrBlob]] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()

      if (cursor.isAfterLast) acc
      else toBlobListHelper(cursor2blobMap(cursor) :: acc)
    }

    @tailrec private def toTypedListHelper[T](field: String, acc: List[T])(implicit getter: CursorGetter[T]): List[T] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T](field, getter.get(cursor, cursor.columnIndex(field)) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2](f1: String, f2: String, acc: List[(T1, T2)])
                                                  (implicit get1: CursorGetter[T1], get2: CursorGetter[T2])
                                                  :List[(T1, T2)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2](f1, f2,
        (get1.get(cursor, cursor.columnIndex(f1)),
         get2.get(cursor, cursor.columnIndex(f2))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3](f1: String, f2: String, f3: String, acc: List[(T1, T2, T3)])
                                                      (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3])
                                                  :List[(T1, T2, T3)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3](f1, f2, f3,
        (get1.get(cursor, cursor.columnIndex(f1)),
         get2.get(cursor, cursor.columnIndex(f2)),
         get3.get(cursor, cursor.columnIndex(f3))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4](f1: String, f2: String, f3: String, f4: String, acc: List[(T1, T2, T3, T4)])
                                                      (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4])
                                                  :List[(T1, T2, T3, T4)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4](f1, f2, f3, f4,
        (get1.get(cursor, cursor.columnIndex(f1)),
         get2.get(cursor, cursor.columnIndex(f2)),
         get3.get(cursor, cursor.columnIndex(f3)),
         get4.get(cursor, cursor.columnIndex(f4))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5](f1: String, f2: String, f3: String, f4: String, f5: String, acc: List[(T1, T2, T3, T4, T5)])
                                                                          (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5])
                                                                          :List[(T1, T2, T3, T4, T5)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5](f1, f2, f3, f4, f5,
        (get1.get(cursor, cursor.columnIndex(f1)),
         get2.get(cursor, cursor.columnIndex(f2)),
         get3.get(cursor, cursor.columnIndex(f3)),
         get4.get(cursor, cursor.columnIndex(f4)),
         get5.get(cursor, cursor.columnIndex(f5))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, acc: List[(T1, T2, T3, T4, T5, T6)])
                                                                          (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6])
                                                                          :List[(T1, T2, T3, T4, T5, T6)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6](f1, f2, f3, f4, f5, f6,
        (get1.get(cursor, cursor.columnIndex(f1)),
         get2.get(cursor, cursor.columnIndex(f2)),
         get3.get(cursor, cursor.columnIndex(f3)),
         get4.get(cursor, cursor.columnIndex(f4)),
         get5.get(cursor, cursor.columnIndex(f5)),
         get6.get(cursor, cursor.columnIndex(f6))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, acc: List[(T1, T2, T3, T4, T5, T6, T7)])
                                                                      (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7])
                                                                      :List[(T1, T2, T3, T4, T5, T6, T7)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7](f1, f2, f3, f4, f5, f6, f7,
        (get1.get(cursor, cursor.columnIndex(f1)),
         get2.get(cursor, cursor.columnIndex(f2)),
         get3.get(cursor, cursor.columnIndex(f3)),
         get4.get(cursor, cursor.columnIndex(f4)),
         get5.get(cursor, cursor.columnIndex(f5)),
         get6.get(cursor, cursor.columnIndex(f6)),
         get7.get(cursor, cursor.columnIndex(f7))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, acc: List[(T1, T2, T3, T4, T5, T6, T7, T8)])
                                                                          (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8])
                                                                          :List[(T1, T2, T3, T4, T5, T6, T7, T8)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8](f1, f2, f3, f4, f5, f6, f7, f8,
        (get1.get(cursor, cursor.columnIndex(f1)),
         get2.get(cursor, cursor.columnIndex(f2)),
         get3.get(cursor, cursor.columnIndex(f3)),
         get4.get(cursor, cursor.columnIndex(f4)),
         get5.get(cursor, cursor.columnIndex(f5)),
         get6.get(cursor, cursor.columnIndex(f6)),
         get7.get(cursor, cursor.columnIndex(f7)),
         get8.get(cursor, cursor.columnIndex(f8))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, acc: List[(T1, T2, T3, T4, T5, T6, T7, T8, T9)])
                                                                              (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9])
                                                                              :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9](f1, f2, f3, f4, f5, f6, f7, f8, f9,
        (get1.get(cursor, cursor.columnIndex(f1)),
         get2.get(cursor, cursor.columnIndex(f2)),
         get3.get(cursor, cursor.columnIndex(f3)),
         get4.get(cursor, cursor.columnIndex(f4)),
         get5.get(cursor, cursor.columnIndex(f5)),
         get6.get(cursor, cursor.columnIndex(f6)),
         get7.get(cursor, cursor.columnIndex(f7)),
         get8.get(cursor, cursor.columnIndex(f8)),
         get9.get(cursor, cursor.columnIndex(f9))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, acc: List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)])
                                                                                   (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10])
                                                                                   :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10,
        (get1.get(cursor, cursor.columnIndex(f1)),
          get2.get(cursor, cursor.columnIndex(f2)),
          get3.get(cursor, cursor.columnIndex(f3)),
          get4.get(cursor, cursor.columnIndex(f4)),
          get5.get(cursor, cursor.columnIndex(f5)),
          get6.get(cursor, cursor.columnIndex(f6)),
          get7.get(cursor, cursor.columnIndex(f7)),
          get8.get(cursor, cursor.columnIndex(f8)),
          get9.get(cursor, cursor.columnIndex(f9)),
          get10.get(cursor, cursor.columnIndex(f10))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, acc: List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)])
                                                                                        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11])
                                                                                        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11,
        (get1.get(cursor, cursor.columnIndex(f1)),
          get2.get(cursor, cursor.columnIndex(f2)),
          get3.get(cursor, cursor.columnIndex(f3)),
          get4.get(cursor, cursor.columnIndex(f4)),
          get5.get(cursor, cursor.columnIndex(f5)),
          get6.get(cursor, cursor.columnIndex(f6)),
          get7.get(cursor, cursor.columnIndex(f7)),
          get8.get(cursor, cursor.columnIndex(f8)),
          get9.get(cursor, cursor.columnIndex(f9)),
          get10.get(cursor, cursor.columnIndex(f10)),
          get11.get(cursor, cursor.columnIndex(f11))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, acc: List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)])
                                                                                        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12])
                                                                                        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12,
        (get1.get(cursor, cursor.columnIndex(f1)),
          get2.get(cursor, cursor.columnIndex(f2)),
          get3.get(cursor, cursor.columnIndex(f3)),
          get4.get(cursor, cursor.columnIndex(f4)),
          get5.get(cursor, cursor.columnIndex(f5)),
          get6.get(cursor, cursor.columnIndex(f6)),
          get7.get(cursor, cursor.columnIndex(f7)),
          get8.get(cursor, cursor.columnIndex(f8)),
          get9.get(cursor, cursor.columnIndex(f9)),
          get10.get(cursor, cursor.columnIndex(f10)),
          get11.get(cursor, cursor.columnIndex(f11)),
          get12.get(cursor, cursor.columnIndex(f12))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String, acc: List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)])
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12], get13: CursorGetter[T13])
        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13,
        (get1.get(cursor, cursor.columnIndex(f1)),
          get2.get(cursor, cursor.columnIndex(f2)),
          get3.get(cursor, cursor.columnIndex(f3)),
          get4.get(cursor, cursor.columnIndex(f4)),
          get5.get(cursor, cursor.columnIndex(f5)),
          get6.get(cursor, cursor.columnIndex(f6)),
          get7.get(cursor, cursor.columnIndex(f7)),
          get8.get(cursor, cursor.columnIndex(f8)),
          get9.get(cursor, cursor.columnIndex(f9)),
          get10.get(cursor, cursor.columnIndex(f10)),
          get11.get(cursor, cursor.columnIndex(f11)),
          get12.get(cursor, cursor.columnIndex(f12)),
          get13.get(cursor, cursor.columnIndex(f13))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String, f14: String, acc: List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)])
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12], get13: CursorGetter[T13], get14: CursorGetter[T14])
        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14,
        (get1.get(cursor, cursor.columnIndex(f1)),
          get2.get(cursor, cursor.columnIndex(f2)),
          get3.get(cursor, cursor.columnIndex(f3)),
          get4.get(cursor, cursor.columnIndex(f4)),
          get5.get(cursor, cursor.columnIndex(f5)),
          get6.get(cursor, cursor.columnIndex(f6)),
          get7.get(cursor, cursor.columnIndex(f7)),
          get8.get(cursor, cursor.columnIndex(f8)),
          get9.get(cursor, cursor.columnIndex(f9)),
          get10.get(cursor, cursor.columnIndex(f10)),
          get11.get(cursor, cursor.columnIndex(f11)),
          get12.get(cursor, cursor.columnIndex(f12)),
          get13.get(cursor, cursor.columnIndex(f13)),
          get14.get(cursor, cursor.columnIndex(f14))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String, f14: String, f15: String, acc: List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)])
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12], get13: CursorGetter[T13], get14: CursorGetter[T14], get15: CursorGetter[T15])
        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15,
        (get1.get(cursor, cursor.columnIndex(f1)),
          get2.get(cursor, cursor.columnIndex(f2)),
          get3.get(cursor, cursor.columnIndex(f3)),
          get4.get(cursor, cursor.columnIndex(f4)),
          get5.get(cursor, cursor.columnIndex(f5)),
          get6.get(cursor, cursor.columnIndex(f6)),
          get7.get(cursor, cursor.columnIndex(f7)),
          get8.get(cursor, cursor.columnIndex(f8)),
          get9.get(cursor, cursor.columnIndex(f9)),
          get10.get(cursor, cursor.columnIndex(f10)),
          get11.get(cursor, cursor.columnIndex(f11)),
          get12.get(cursor, cursor.columnIndex(f12)),
          get13.get(cursor, cursor.columnIndex(f13)),
          get14.get(cursor, cursor.columnIndex(f14)),
          get15.get(cursor, cursor.columnIndex(f15))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
      f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String, f14: String, f15: String, f16: String, acc: List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)])
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12], get13: CursorGetter[T13], get14: CursorGetter[T14], get15: CursorGetter[T15], get16: CursorGetter[T16])
        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16,
        (get1.get(cursor, cursor.columnIndex(f1)),
          get2.get(cursor, cursor.columnIndex(f2)),
          get3.get(cursor, cursor.columnIndex(f3)),
          get4.get(cursor, cursor.columnIndex(f4)),
          get5.get(cursor, cursor.columnIndex(f5)),
          get6.get(cursor, cursor.columnIndex(f6)),
          get7.get(cursor, cursor.columnIndex(f7)),
          get8.get(cursor, cursor.columnIndex(f8)),
          get9.get(cursor, cursor.columnIndex(f9)),
          get10.get(cursor, cursor.columnIndex(f10)),
          get11.get(cursor, cursor.columnIndex(f11)),
          get12.get(cursor, cursor.columnIndex(f12)),
          get13.get(cursor, cursor.columnIndex(f13)),
          get14.get(cursor, cursor.columnIndex(f14)),
          get15.get(cursor, cursor.columnIndex(f15)),
          get16.get(cursor, cursor.columnIndex(f16))) +: acc)
    }

    @tailrec private def toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
      f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String, f14: String, f15: String, f16: String, f17: String, f18: String, acc: List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)])
        (implicit get1: CursorGetter[T1], get2: CursorGetter[T2], get3: CursorGetter[T3], get4: CursorGetter[T4], get5: CursorGetter[T5], get6: CursorGetter[T6], get7: CursorGetter[T7], get8: CursorGetter[T8], get9: CursorGetter[T9], get10: CursorGetter[T10], get11: CursorGetter[T11], get12: CursorGetter[T12], get13: CursorGetter[T13], get14: CursorGetter[T14], get15: CursorGetter[T15], get16: CursorGetter[T16], get17: CursorGetter[T17], get18: CursorGetter[T18])
        :List[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()
      if (cursor.isAfterLast) acc
      else toTypedListHelper[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
        f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18,
        (get1.get(cursor, cursor.columnIndex(f1)),
          get2.get(cursor, cursor.columnIndex(f2)),
          get3.get(cursor, cursor.columnIndex(f3)),
          get4.get(cursor, cursor.columnIndex(f4)),
          get5.get(cursor, cursor.columnIndex(f5)),
          get6.get(cursor, cursor.columnIndex(f6)),
          get7.get(cursor, cursor.columnIndex(f7)),
          get8.get(cursor, cursor.columnIndex(f8)),
          get9.get(cursor, cursor.columnIndex(f9)),
          get10.get(cursor, cursor.columnIndex(f10)),
          get11.get(cursor, cursor.columnIndex(f11)),
          get12.get(cursor, cursor.columnIndex(f12)),
          get13.get(cursor, cursor.columnIndex(f13)),
          get14.get(cursor, cursor.columnIndex(f14)),
          get15.get(cursor, cursor.columnIndex(f15)),
          get16.get(cursor, cursor.columnIndex(f16)),
          get17.get(cursor, cursor.columnIndex(f17)),
          get18.get(cursor, cursor.columnIndex(f18))) +: acc)
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
