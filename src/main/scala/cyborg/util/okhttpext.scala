package cyborg.util

import scalaz._
import argonaut._

import com.squareup.okhttp.ResponseBody
import com.squareup.okhttp.RequestBody
import com.squareup.okhttp.MediaType
import com.squareup.okhttp.FormEncodingBuilder

import cyborg.exceptions._
import cyborg.util.scalazext._

object okhttpext {
  trait OkHttpMediaType {
    def value: MediaType
  }

  object OkHttpMediaTypes {
    case object PlainText extends OkHttpMediaType {
      lazy override val value = MediaType.parse("text/plain; charset=utf-8")
    }
    case object Json extends OkHttpMediaType {
      lazy override val value = MediaType.parse("application/json; charset=utf-8")
    }
  }


  trait OkHttpResponseBodyConverter[A] {
    def readAs(body: ResponseBody): Throwable \/ A
  }

  implicit class OkHttpResponseBodyConverterMethods(val body: ResponseBody)
      extends AnyVal {
    def readAs[A](implicit C: OkHttpResponseBodyConverter[A]) = C.readAs(body)
  }

  implicit object StringOkHttpResponseBodyConverter
      extends OkHttpResponseBodyConverter[String] {
    override def readAs(body: ResponseBody): Throwable \/ String =
      \/.fromTryCatchNull(body.string)
  }

  implicit object JsonOkHttpResponseBodyConverter
      extends OkHttpResponseBodyConverter[Json] {
    override def readAs(body: ResponseBody): Throwable \/ Json =
      \/.fromTryCatchNull(body.string).flatMap { s =>
        Parse.parse(s).leftMap(e => JsonParseError(e, Some(s)))
      }
  }


  trait OkHttpRequestBodyMaker[A] {
    def make(data: A): RequestBody
  }

  def makeRequestBody[A](data: A)
    (implicit M: OkHttpRequestBodyMaker[A]): RequestBody = M.make(data)

  implicit object StringOkHttpRequestBodyMaker
      extends OkHttpRequestBodyMaker[String] {
    def make(data: String): RequestBody =
      RequestBody.create(OkHttpMediaTypes.PlainText.value, data)
  }

  implicit object JsonOkHttpRequestBodyMaker
      extends OkHttpRequestBodyMaker[Json] {
    def make(data: Json): RequestBody =
      RequestBody.create(OkHttpMediaTypes.Json.value, data.nospaces)
  }

  implicit object FormOkHttpRequestBodyMaker
      extends OkHttpRequestBodyMaker[List[(String, String)]] {
    def make(data: List[(String, String)]): RequestBody =
      data.foldLeft(new FormEncodingBuilder()) { (builder, pair) =>
        builder.add(pair._1, pair._2)
      } .build()
  }

}
