package cml

import Attributes._
import cml.xml._, Xml._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.ScalazProperties

object AtomTest extends Properties("Atom") with Generators {

  property("equal laws") = ScalazProperties.equal.laws[Atom]

  property("write / read xml") = forAll { a: Atom ⇒ 
    a.xml(AtomQn).read[Atom] ≟ a.success
  }
}

// vim: set ts=2 sw=2 et:
