package cml

import org.scalacheck._, Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._

trait Generators {

  lazy val atomsG = Gen listOf arb[Atom]

  lazy val formalChargeG = optionGen(Gen choose (-9, 9))

  lazy val countG = optionGen(Gen choose (0D, 100D))

  lazy val idG = Gen.identifier

  implicit lazy val ElementArbitrary: Arbitrary[Element] =
    Arbitrary(Gen oneOf Element.values)

  implicit lazy val AtomArbitrary: Arbitrary[Atom] = Arbitrary(
    ^^(idG, arb[Element], formalChargeG)(Atom.apply)
  )

  implicit lazy val MoleculeArbitrary: Arbitrary[Molecule] = Arbitrary(
    ^^(idG, countG, atomsG)(Molecule.apply)
  )

  def optionGen[A](ga: Gen[A]): Gen[Option[A]] =
    Gen.frequency((1, none[A]), (9, ga map { _.some }))
}

// vim: set ts=2 sw=2 et:
