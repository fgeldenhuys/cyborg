package cyborg

import android.os.{Parcel => P}
import cyborg.util.control._

object Parcel {
  implicit class CyborgParcelExt(val p: P) extends AnyVal {
    def read[T](implicit t: ParcelValue[T]): Option[T] = tryOption(t read p)
    def write[T](value: T)(implicit t: ParcelValue[T]): Option[P] = tryOption { t.write(p, value); p }
    def check[T](expected: T)(implicit t: ParcelValue[T]): Boolean = tryOption(t.read(p) == expected) getOrElse false
  }

  trait ParcelValue[T] {
    def read(p: P): T
    def write(p: P, value: T)
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
