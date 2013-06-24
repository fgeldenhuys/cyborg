package cyborg.db

import android.database.sqlite.{SQLiteDatabase => ASQLD}
import android.database.{Cursor => AC}

object SQLite {
  type StringOrBlob = Either[String, Array[Byte]]

  implicit class Database(val db: ASQLD) {
    def apply(sql: String, args: String*) = db.rawQuery(sql, args.toArray)
  }

  implicit class Cursor(val cursor: AC) {
    def apply(columnName: String): Option[String] = {
      if (cursor.getCount > 0) {
        if (cursor.isBeforeFirst) cursor.moveToFirst()
        Option(cursor.getString(cursor.getColumnIndex(columnName)))
      }
      else None
    }

    def blob(columnName: String): Option[Array[Byte]] = {
      if (cursor.getCount > 0) {
        if (cursor.isBeforeFirst) cursor.moveToFirst()
        Some(cursor.getBlob(cursor.getColumnIndex(columnName)))
      }
      else None
    }

    def isEmpty: Boolean = cursor.getCount == 0

    def toList: List[Map[String, String]] = {
      cursor.moveToPosition(-1)
      val result = toListHelper
      cursor.close()
      result
    }

    def toBlobList: List[Map[String, StringOrBlob]] = {
      cursor.moveToPosition(-1)
      val result = toBlobListHelper
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

    private def toListHelper: List[Map[String, String]] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()

      if (cursor.isAfterLast) Nil
      else cursor2map(cursor) :: toListHelper
    }

    private def toBlobListHelper: List[Map[String, StringOrBlob]] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()

      if (cursor.isAfterLast) Nil
      else cursor2blobMap(cursor) :: toBlobListHelper
    }
  }
}
