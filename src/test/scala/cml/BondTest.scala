package cml

import Attributes._
import cml.xml._, Xml._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.ScalazProperties

object BondTest extends Properties("Bond") with Generators {

  property("equal laws") = ScalazProperties.equal.laws[Bond]

  property("write / read xml") = forAll { a: Bond ⇒ 
    a.xml(BondQn).readV[Bond] ≟ a.success
  }
}

// vim: set ts=2 sw=2 et:
