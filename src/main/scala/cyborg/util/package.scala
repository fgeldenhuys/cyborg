package cyborg

package object util {
  case class NotImplemented(message: String = "Not Implemented") extends Exception(message)
  case class InvalidConversion(message: String = "Invalid Conversion") extends Exception(message)
}
