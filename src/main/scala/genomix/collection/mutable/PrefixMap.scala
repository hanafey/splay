package genomix.collection.mutable

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.generic.CanBuildFrom

/**
 * Closely derived from the Programming Scala book, as presented in the official Scala web site.
 *
 * @tparam T
 */
class PrefixMap[T] extends mutable.Map[String, T] with mutable.MapLike[String, T, PrefixMap[T]] {

  var suffixes: Map[Char, PrefixMap[T]] = Map.empty
  var value: Option[T] = None

  override def empty = new PrefixMap[T]

  @tailrec
  final def withPrefix(prefix: String): PrefixMap[T] = {
    if (prefix.isEmpty) {
      this
    } else {
      val first = prefix(0)
      val nextSuffixes = suffixes.get(first) match {
        case None => {
          val e = empty
          suffixes = suffixes + (first -> e)
          e
        }
        case Some(x) => x
      }
      nextSuffixes.withPrefix(prefix.substring(1))
    }
  }

  override def update(key: String, value: T): Unit = {
    withPrefix(key).value = Option(value)
  }

  @tailrec
  override final def remove(key: String): Option[T] = {
    if (key.isEmpty) {
      val prev = value
      value = None
      prev
    } else {
      suffixes.get(key(0)) match {
        case None => None
        case Some(x) => x.remove(key.substring(1))
      }
    }
  }

  def +=(kv: (String, T)): this.type = {
    update(kv._1, kv._2)
    this
  }

  def -=(key: String): this.type = {
    remove(key)
    this
  }

  @tailrec
  final def get(key: String): Option[T] = {
    if (key.isEmpty) {
      value
    } else {
      suffixes.get(key(0)) match {
        case Some(x) => x.get(key.substring(1))
        case None => None
      }
    }
  }

  def iterator: Iterator[(String, T)] = {
    (for (v <- value.iterator) yield ("", v)) ++
      (for {
        (chr, pm) <- suffixes.iterator
        (s, v) <- pm.iterator
      } yield (chr +: s, v))
  }

  def it: Iterator[(String, T)] = {
    value.iterator.map(v => ("", v)) ++
      suffixes.iterator.flatMap {
        case (chr, pm) => pm.iterator.map {
          case (key, v) => (chr +: key, v)
        }
      }
  }
}

object PrefixMap {

  def empty[T]: PrefixMap[T] = new PrefixMap[T]

  def apply[T](elements: (String, T)*): PrefixMap[T] = {
    val me = empty[T]
    for ((k, v) <- elements) me.update(k, v)
    me
  }

  def newBuilder[T]: mutable.Builder[(String, T), PrefixMap[T]] = new mutable.MapBuilder[String, T, PrefixMap[T]](empty)

  implicit def canBuildFrom[T]: CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] =
    new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {
      def apply(from: PrefixMap[_]) = newBuilder[T]

      def apply(): mutable.Builder[(String, T), PrefixMap[T]] = newBuilder[T]
    }
}