package cyborg

object Conversions {
  implicit def fun2runnable(f: => Unit): Runnable = new Runnable { def run() { f } }
}
