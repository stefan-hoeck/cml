package cml

import Attributes._
import cml.xml._, Xml._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.ScalazProperties

object MoleculeTest extends Properties("Molecule") with Generators {

  property("equal laws") = ScalazProperties.equal.laws[Molecule]

  property("write / read xml") = forAll { m: Molecule ⇒ 
    m.xml(MoleculeQn).read[Molecule] ≟ m.success
  }
}

// vim: set ts=2 sw=2 et:
