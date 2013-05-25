package cml

import org.scalacheck._, Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._

trait Generators {

  lazy val atomsG = Gen listOf arb[Atom]

  lazy val atomRefs2G: Gen[AtomRefs2] = ^(idG, idG)(Pair.apply)

  lazy val bondsG = Gen listOf arb[Bond]

  lazy val formalChargeG = optionGen(Gen choose (-9, 9))

  lazy val countG = optionGen(Gen choose (0D, 100D))

  lazy val idG = Gen.identifier

  implicit lazy val BondOrderArbitrary: Arbitrary[BondOrder] =
    Arbitrary(Gen oneOf BondOrder.values)

  implicit lazy val BondArbitrary: Arbitrary[Bond] = Arbitrary(
    ^^(idG map { _.some }, atomRefs2G, arb[BondOrder])(Bond.apply)
  )

  implicit lazy val ElementArbitrary: Arbitrary[Element] =
    Arbitrary(Gen oneOf Element.values)

  implicit lazy val AtomArbitrary: Arbitrary[Atom] = Arbitrary(
    ^^(idG, arb[Element], formalChargeG)(Atom.apply)
  )

  implicit lazy val MoleculeArbitrary: Arbitrary[Molecule] = Arbitrary(
    ^^^(idG, countG, atomsG, bondsG)(Molecule.apply)
  )

  def optionGen[A](ga: Gen[A]): Gen[Option[A]] =
    Gen.frequency((1, none[A]), (9, ga map { _.some }))
}

// vim: set ts=2 sw=2 et:
