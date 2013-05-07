package cml

import cml.xml.Read
import org.scalacheck._
import scalaz._, Scalaz._, scalacheck.ScalazProperties

object ElementTest extends Properties("Element") with Generators {

  val count = Element.values.size

  property("unique symbols") = 
    Element.values.map { _.symbol }.toSet.size ≟ count

  property("unique, increasing order numbers") = 
    Element.values.map { _.atomicNr } ≟ (0 until count).toList

  property("find element by symbol") =
    Element.values ∀ { e ⇒ Element.fromSymbol(e.symbol) ≟ e.some }

  property("find element by symbol validated") =
    Element.values ∀ { e ⇒ Element.fromSymbolV(e.symbol) ≟ e.success }

  property("show / read") =
    Element.values ∀ { e ⇒ Read[Element].read(e.shows) ≟ e.success }

  property("equal laws") = ScalazProperties.equal.laws[Element]
}

// vim: set ts=2 sw=2 et:
