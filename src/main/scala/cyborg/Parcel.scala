package cyborg

import android.os.{Parcel => P, ParcelFormatException}
import scalaz._, Scalaz._

object Parcel {
  implicit class CyborgParcelExt(val p: P) extends AnyVal {
    def read[T](implicit t: ParcelValue[T]): Throwable \/ T =
      \/.fromTryCatch(t read p)
    def write[T](value: T)(implicit t: ParcelValue[T]): P = {
      t.write(p, value)
      p
    }
    def check[T](expected: T)(implicit t: ParcelValue[T]): Throwable \/ T =
      \/.fromTryCatch {
        val value = t read p
        if (value != expected) throw new ParcelFormatException(s"Expected '$expected' from parcel but got '$value'")
        value
      }
  }

  trait ParcelValue[T] {
    def read(p: P): T
    def write(p: P, value: T): Unit
  }

  implicit val parcelIntValue = new ParcelValue[Int] {
    def read(p: P) = p.readInt
    def write(p: P, s: Int) = p writeInt s
  }

  implicit val parcelStringValue = new ParcelValue[String] {
    def read(p: P) = p.readString
    def write(p: P, s: String) = p writeString s
  }

  implicit val parcelByteArrayValue = new ParcelValue[Array[Byte]] {
    def read(p: P) = p.createByteArray()
    def write(p: P, bytes: Array[Byte]) = p.writeByteArray(bytes)
  }
}
