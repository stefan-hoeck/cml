package cml

import cml.xml.Read
import org.scalacheck._
import scalaz._, Scalaz._, scalacheck.ScalazProperties

object BondOrderTest extends Properties("BondOrder") with Generators {

  val count = BondOrder.values.size

  property("unique symbols") = 
    BondOrder.values.map { _.symbol }.toSet.size ≟ count

  property("find element by symbol") =
    BondOrder.values ∀ { e ⇒ BondOrder.fromSymbol(e.short) ≟ e.some }

  property("find element by symbol validated") =
    BondOrder.values ∀ { e ⇒ BondOrder.fromSymbolV(e.short) ≟ e.success }

  property("show / read") =
    BondOrder.values ∀ { e ⇒ Read[BondOrder].read(e.shows) ≟ e.success }

  property("equal laws") = ScalazProperties.equal.laws[BondOrder]
}

// vim: set ts=2 sw=2 et:
