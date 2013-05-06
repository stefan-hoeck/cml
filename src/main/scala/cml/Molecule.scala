package cml

import cml.xml._, Xml._
import scalaz._, Scalaz._

case class Molecule(
    id: String,
    count: Option[Double],
    atoms: List[Atom])

object Molecule {
  private def ap[F[_]:Apply] = Apply[F] lift3 Molecule.apply

  implicit val WriteXmlImpl = writeData { m: Molecule ⇒ 
    m.id.writeAttr(IdQn) ⊹ 
    m.count.foldMap { _ writeAttr CountQn } ⊹ 
    m.atoms.foldMap { _.elemData }
  }
}



// vim: set ts=2 sw=2 et:
