package cyborg.util

import java.io.File
import java.nio.ByteBuffer
import java.util.zip.{Adler32 => Adler32J}

import scalaz._, Scalaz._
import argonaut._, Argonaut._

import cyborg.util.binary._
import cyborg.util.io._
import cyborg.exceptions._

object checksum {
  case class SHA1 private (value: Array[Byte]) {
    lazy val asHexString = value.toHexString
  }
  object SHA1 {
    def fromByteArray(b: Array[Byte]): Option[SHA1] = (b.length == 20).option(SHA1(b))
    def fromHexString(s: String): Option[SHA1] =
      byteArrayFromHexString(s).toOption.flatMap(b => (b.length == 20).option(b)).map(SHA1(_))
  }

  case class Adler32 private (value: Array[Byte]) {
    lazy val asHexString = value.toHexString
    lazy val asLong = ByteBuffer.wrap(value).getInt.toLong & 0xffffffffl
  }
  object Adler32 {
    def fromHexString(s: String): Option[Adler32] =
      byteArrayFromHexString(s).toOption.flatMap(b => (b.length == 4).option(b)).map(Adler32(_))
    def fromLong(l: Long): Adler32 = {
      Adler32(ByteBuffer.allocate(8).putLong(l).array.drop(4))
    }
  }

  implicit def Adler32DecodeJson: DecodeJson[Adler32] = DecodeJson(c =>
    if (c.focus.isString)
      c.focus.as[String].map(Adler32.fromHexString).flatMap(_.cata(
        DecodeResult.ok,
        DecodeResult.fail("String is not a valid Adler32 checksum", c.history)))
    else if (c.focus.isNumber)
      c.focus.as[Long].map(Adler32.fromLong)
    else {
      println("adler32 decode fail " + c)
      DecodeResult.fail("Could not get Adler32 from this field", c.history)
    }
  )

  implicit object Adler32Equal extends Equal[Adler32] {
    override def equal(a: Adler32, b: Adler32): Boolean = a.value === b.value
  }

  implicit object SHA1Equal extends Equal[SHA1] {
    override def equal(a: SHA1, b: SHA1): Boolean = a.value === b.value
  }

  trait ChecksumMaker[A, C] {
    def apply(a: A): Throwable \/ C
  }

  implicit object SHA1FromFileMaker extends ChecksumMaker[File, SHA1] {
    override def apply(a: File): Throwable \/ SHA1 = {
      inputStream(a).flatMap { in =>
        val md = java.security.MessageDigest.getInstance("SHA-1")
        inputStreamByteArrayUpdateReader(in)((buf, n) =>
          md.update(buf, 0, n)
        ).flatMap(_ => SHA1.fromByteArray(md.digest) \/> InvalidInputLength("SHA1 may only be 20 bytes long"))
      }
    }
  }

  implicit object Adler32FromStringMaker extends ChecksumMaker[String, Adler32] {
    override def apply(a: String): Throwable \/ Adler32 = {
      \/.fromTryCatchNonFatal {
        val adler = new Adler32J
        adler.update(a.getBytes("UTF-8"))
        Adler32.fromLong(adler.getValue)
      }
    }
  }

  implicit object Adler32FromFileMaker extends ChecksumMaker[File, Adler32] {
    override def apply(a: File): Throwable \/ Adler32 = {
      inputStream(a).flatMap { in =>
        val adler = new Adler32J
        inputStreamByteArrayUpdateReader(in)((buf, n) =>
          adler.update(buf, 0, n)
        ).map(_ => Adler32.fromLong(adler.getValue))
      }
    }
  }

  implicit object Adler32FromThrowableOrStringMaker
      extends ChecksumMaker[Throwable \/ String, Adler32] {
    override def apply(a: Throwable \/ String): Throwable \/ Adler32 = {
      a.flatMap(Adler32FromStringMaker.apply)
    }
  }

  implicit object Adler32FromThrowableOrFileMaker
      extends ChecksumMaker[Throwable \/ File, Adler32] {
    override def apply(a: Throwable \/ File): Throwable \/ Adler32 = {
      a.flatMap(Adler32FromFileMaker.apply)
    }
  }

  implicit class ChecksumMakerOps[A](val a: A) extends AnyVal {
    def calculateChecksum[C](implicit M: ChecksumMaker[A, C]) = M.apply(a)
    def verifyChecksum[C: Equal](c: C)(implicit M: ChecksumMaker[A, C]): Throwable \/ Boolean =
      M.apply(a).map(x => x === c)
  }


}
