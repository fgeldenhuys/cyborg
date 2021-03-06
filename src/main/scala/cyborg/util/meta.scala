package cyborg.util

// https://michid.wordpress.com/code/meta-programming-with-scala-conditional-compilation/

import scala.language.higherKinds

object meta {
  object Booleans {
    trait BOOL {
      type a[t <: BOOL, f <: BOOL] <: BOOL
      type v = a[TRUE, FALSE]
    }
    final class TRUE extends BOOL {
      type a[t <: BOOL, f <: BOOL] = t
    }
    final class FALSE extends BOOL{
      type a[t <: BOOL, f <: BOOL] = f
    }
    trait IF[x <: BOOL, y <: BOOL, z <: BOOL] extends BOOL {
      type a[t <: BOOL, f <: BOOL] = x#a[y, z]#a[t, f]
    }
    trait NOT[x <: BOOL] extends BOOL {
      type a[t <: BOOL, f <: BOOL] = IF[x, FALSE, TRUE]#a[t, f]
    }
    trait AND[x <: BOOL, y <: BOOL] extends BOOL {
      type a[t <: BOOL, f <: BOOL] = IF[x, y, FALSE]#a[t, f]
    }
    trait OR[x <: BOOL, y <: BOOL] extends BOOL {
      type a[t <: BOOL, f <: BOOL] = IF[x, TRUE, y]#a[t, f]
    }
    // Needs -Yrecursion 1
    //trait XOR[x <: BOOL, y <: BOOL] extends BOOL {
    //  type a[t <: BOOL, f <: BOOL] = ((x || y) && ![x && y])#a[t, f]
    //}

    type ![x <: BOOL] = NOT[x]
    type ||[x <: BOOL, y <: BOOL] = OR[x, y]
    type &&[x <: BOOL, y <: BOOL] = AND[x, y]
    // Needs -Yrecursion 1
    //type ^^[x <: BOOL, y <: BOOL] = XOR[x, y]
  }

  object PreProc {
    import Booleans._

    def IF[B] = null.asInstanceOf[B]

    object Include {
      def apply(block: => Unit): Unit = {
        block
      }
    }

    object Exclude {
      def apply(block: => Unit): Unit = { }
    }

    implicit def include(t: TRUE) = Include
    implicit def exclude(f: FALSE) = Exclude
  }
}