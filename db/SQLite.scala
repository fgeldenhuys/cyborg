package cyborg.db

import android.database.sqlite.{SQLiteDatabase => ASQLD}
import android.database.{Cursor => AC}

object SQLite {
  implicit class Database(val db: ASQLD) {
    def apply(sql: String, args: String*) = db.rawQuery(sql, args.toArray)
  }

  implicit class Cursor(val cursor: AC) {
    def apply(columnName: String): Option[String] = {
      if (cursor.getCount > 0) {
        if (cursor.isBeforeFirst) cursor.moveToFirst()
        Some(cursor.getString(cursor.getColumnIndex(columnName)))
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
      toListHelper
    }

    private def cursor2map(cursor: AC): Map[String, String] =
      cursor.getColumnNames.map(name =>
        (name, cursor.getString(cursor.getColumnIndex(name)))).toMap

    private def toListHelper: List[Map[String, String]] = {
      if (cursor.isBeforeFirst) cursor.moveToFirst()
      else cursor.moveToNext()

      if (cursor.isAfterLast) Nil
      else cursor2map(cursor) :: toListHelper
    }
  }
}
