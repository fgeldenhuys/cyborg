package cyborg.util

object events {
  trait Observable[A, B] {
    def sub(f: (A) => B): (A) => B
    def unsub(f: (A) => B): Unit
  }

  class Var[T](var value: T) extends Observable[T, Any] {
    val observers = scala.collection.mutable.ListBuffer.empty[(T) => Any]

    def apply() = value
    def update(newValue: T): Unit = {
      value = newValue
      for (o <- observers) o(value)
    }

    def sub(f: (T) => Any): (T) => Any = {
      observers += f
      f
    }
    def unsub(f: (T) => Any): Unit = {
      observers -= f
    }
  }

  implicit def value2observableVar[T](value: T): Var[T] = new Var(value)
}
