package cml

import scalaz._, Scalaz._

final case class Atom(
    id: CMLId,
    element: Element,
    formalCharge: Option[Int])

object Atom {
  implicit val AtomEqual = Equal.equalA[Atom]
}

// vim: set ts=2 sw=2 et:
