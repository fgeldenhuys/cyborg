package cyborg

object exceptions {
  case class AlreadyShutdown(message: String) extends Exception(message)
  case class InvalidInputLength(message: String) extends Exception(message)
  case class InvalidHttpStatusCode(code: Int) extends Exception(s"Invalid HTTP status code $code")
  case class JsonParseError(errors: String, source: Option[String])
      extends Exception(errors)
  case class ChecksumFailed() extends Exception("Checksum failed")
}
