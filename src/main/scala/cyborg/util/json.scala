package cyborg.util

import scalaz._, Scalaz._
import argonaut._, Argonaut._

import cyborg.Log._
import cyborg.util.scalazext._

object json {
  implicit class JsonCyborgExt(val json: Json) extends AnyVal {
    def fieldAs[A](name: String)(implicit D: DecodeJson[A]): Option[A] = {
      json.field(name)
          .flatMap(_.as[A](D).fold[Option[A]]((failure, _) => {
            $w("Failed to decode json value: " + failure)
            None
          }, Some(_)))
    }
  }
}
