package cml

import cml.xml._, Xml._
import scalaz._, Scalaz._

final case class Atom(
    id: CMLId,
    element: Element,
    formalCharge: Option[Int])

object Atom {
  private def ap[F[_]:Apply] = Apply[F] lift3 Atom.apply

  implicit val AtomEqual = Equal.equalA[Atom]

  implicit val ReadXmlImpl = fromElem { e ⇒ 
    ap[ValRes] apply (e.read, e.read, e readAttrO FormalChargeQn)
  }

  implicit val WriteXmlImpl = elemData { a: Atom ⇒ 
    a.id.elemData ⊹ 
    a.element.elemData ⊹ 
    a.formalCharge.foldMap { _ writeAttr FormalChargeQn }
  }
}

// vim: set ts=2 sw=2 et:
