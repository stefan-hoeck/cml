package cml

import Attributes._
import cml.xml._, Xml._
import scalaz._, Scalaz._

final case class Atom(
    id: String,
    element: Element,
    formalCharge: Option[Int])

object Atom {
  implicit val AtomEqual = Equal.equalA[Atom]

  implicit val XmlReaderImpl: XmlReader[Atom] = 
    ^^(rId, rElement, rFormalCharge)(Atom.apply)

  implicit val XmlWriterImpl: XmlWriter[Atom] = a ⇒ 
    wId(a.id) ⊹ wElement(a.element) ⊹ wFormalCharge(a.formalCharge)
}

// vim: set ts=2 sw=2 et:
