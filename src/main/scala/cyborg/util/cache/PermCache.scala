package cyborg.util.cache

class PermCache[K, V] private (load: K => V) {
  @volatile private var cache = Map.empty[K, V]

  def apply(k: K): V = cache.getOrElse(k, {
    val miss = load(k)
    cache = cache + ((k, miss))
    miss
  })

  def clear(): Unit = {
    cache = Map.empty[K, V]
  }
}

object PermCache {
  def makePermCache[K, V](load: K => V) = new PermCache[K, V](load)
}
