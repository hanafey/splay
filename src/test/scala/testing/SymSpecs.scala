package testing

import org.specs2.mutable._
import genomix.{SymCompanion, Sym}

class SymSpecs extends Specification {
  val a = "abc"
  val b = "ab" + 'c'
  val c = "abcd"
  "Base class Sym" should {
    "behave" in {
      val aSym = Sym(a)
      aSym.name mustEqual a
      aSym.toString mustEqual a

      val bSym = Sym(b)
      a must not beTheSameAs b
      aSym mustEqual bSym
      aSym must beTheSameAs(bSym)
    }
  }

  "Subclass Marker" should {
    "behave" in {
      val aMarker = Marker(a)
      aMarker.name mustEqual a
      aMarker.toString mustEqual a

      val bMarker = Marker(b)
      a must not beTheSameAs b
      aMarker must beTheSameAs(bMarker)
      aMarker mustEqual bMarker

      val aSym = Sym(a)
      aSym.name mustEqual aMarker.name
      aSym mustNotEqual (aMarker)

    }
  }

}

class Marker private(name: String) extends Sym(name)

object Marker extends SymCompanion[Marker] {
  protected def valueFromKey(k: String): Marker = new Marker(k)

  protected def keyFromValue(v: Marker): Option[String] = Some(v.name)
}


