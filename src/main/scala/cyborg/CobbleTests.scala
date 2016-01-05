package cyborg

object CobbleTests {
  // TODO: you should be able to turn all this off

  object Cobble {
    class PartialAssertion[A](x: => A) {
      def equals[B <: A](y: => B) = new EqualsAssertion(x, y)
    }
    class EqualsAssertion[A, B](x: => A, y: => B) {
      val passed = x == y
      def otherwise(message: String): Unit = {
        if (!passed) {
          cyborg.Log.$e(" ALERT! " + message)
          cyborg.Log.$e("   GOT: " + x.toString)
          cyborg.Log.$e("EXPECT: " + y.toString)
        }
      }
    }
  }

  // TODO: make this a macro that captures file and line
  def test[A](x: => A): Cobble.PartialAssertion[A] = new Cobble.PartialAssertion(x)
}