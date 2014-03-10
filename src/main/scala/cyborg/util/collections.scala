package cyborg.util

import scala.util.Random

object collections {
  implicit class SeqCollectionsCyborgExt[A](val xs: Seq[A]) extends AnyVal {
    def randomItem(implicit random: Random): Option[A] = {
      if (xs.isEmpty) None
      else Some(xs(random.nextInt(xs.size)))
    }
  }
}
