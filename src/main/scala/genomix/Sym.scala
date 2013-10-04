package genomix

import java.util

/**
 * Just like the [[scala.Symbol]] it was copied from except it can be sub-classed to provide type-safe sets of
 * symbols.
 *
 * Sub-classes must follow the following recipe:
 *
 * <code>
 * import genomix.{SymCompanion, Sym, UniquenessCache}

class MarkerSym protected (symbol:String) extends Sym(symbol)

object MarkerSym extends SymCompanion[MarkerSym] {
  protected def valueFromKey(name: String): MarkerSym= new MarkerSym(name)
  protected def keyFromValue(sym: MarkerSym): Option[String] = Some(sym.name)
}

 * </code>
 * @param name The symbol name
 */
class Sym protected(val name: String) extends Serializable {

  /** Converts this symbol to a string.
    */
  override def toString: String = name

  override val hashCode = name.hashCode()

  override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
}

abstract class SymCompanion[T >: Null] extends UniquenessCache[String, T] {
  override def apply(name: String): T = super.apply(name)
}

object Sym extends SymCompanion[Sym] {
  protected def valueFromKey(name: String): Sym = new Sym(name)

  protected def keyFromValue(sym: Sym): Option[String] = Some(sym.name)
}

private[genomix] abstract class UniquenessCache[K, V >: Null] {

  import java.lang.ref.WeakReference
  import java.util.concurrent.locks.ReentrantReadWriteLock

  private val rwl = new ReentrantReadWriteLock()
  private val rlock = rwl.readLock
  private val wlock = rwl.writeLock
  private val map = new util.WeakHashMap[K, WeakReference[V]]

  protected def valueFromKey(k: K): V

  protected def keyFromValue(v: V): Option[K]

  def apply(name: K): V = {
    def cached(): V = {
      rlock.lock()
      try {
        val reference = map get name
        if (reference == null) null
        else reference.get // will be null if we were gc-ed
      }
      finally rlock.unlock()
    }
    def updateCache(): V = {
      wlock.lock()
      try {
        val res = cached()
        if (res != null) res
        else {
          // If we don't remove the old String key from the map, we can
          // wind up with one String as the key and a different String as
          // as the name field in the Symbol, which can lead to surprising
          // GC behavior and duplicate Symbols. See SI-6706.
          map remove name
          val sym = valueFromKey(name)
          map.put(name, new WeakReference(sym))
          sym
        }
      }
      finally wlock.unlock()
    }

    val res = cached()
    if (res == null) updateCache()
    else res
  }

  def unapply(other: V): Option[K] = keyFromValue(other)
}
