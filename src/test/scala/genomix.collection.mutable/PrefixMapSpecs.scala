package genomix.collection.mutable

import org.specs2.mutable._

class PrefixMapSpecs extends Specification {
  val keys = List("a", "b", "c", "aa", "ab", "abc")
  val values = List(1, 2, 3, 11, 12, 123)
  val zip: List[(String, Int)] = keys.zip(values)

  "PrefixMap" should {
    "Contain only what is put in" in {
      val pm = PrefixMap(zip: _*)

      for ((k, v) <- zip) {
        pm.get(k) mustEqual Some(v)
      }
      for ((k, v) <- zip) {
        pm.get(k.toUpperCase) mustEqual None
      }
      for ((k, v) <- zip) {
        pm(k) mustEqual v
      }
      true
    }

    "Is mutable" in {
      val pmA = PrefixMap.empty[Int]
      for (z <- zip) {
        pmA += z
      }
      pmA mustEqual PrefixMap(zip: _*)

      val pmB = PrefixMap.empty[Int]
      pmB ++= zip
      pmB mustEqual PrefixMap(zip: _*)
      pmB mustEqual pmA
    }

    "Has an iterator" in {
      val pm = PrefixMap(zip: _*)
      val k = collection.mutable.ArrayBuffer.empty[String]
      val v = collection.mutable.ArrayBuffer.empty[Int]
      val it = pm.iterator
      while (it.hasNext) {
        val nv: (String, Int) = it.next
        k += nv._1
        v += nv._2
      }
      k.sorted mustEqual keys.sorted
      v.sorted mustEqual values.sorted
    }
  }

}
