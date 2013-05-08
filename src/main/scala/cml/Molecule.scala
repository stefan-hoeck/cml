package cml

import Attributes._
import cml.xml._, Xml._
import scalaz._, Scalaz._

case class Molecule(
    id: String,
    count: Option[Double],
    atoms: List[Atom]) {

  def shows: String = {
    val as = atoms mkString "  \n"

    s"Molecule{\n  id = $id\n  count = $count\n$as\n}"
  }
}

object Molecule {
  implicit val MoleculeEqual = Equal.equalA[Molecule]

  implicit val MoleculeShow = Show shows { m: Molecule ⇒ m.shows}

  implicit val XmlReaderImpl: XmlReader[Molecule] =
    ^^(rId, rCount, rAtoms)(Molecule.apply)

  implicit val XmlWriterImpl: XmlWriter[Molecule] = m ⇒ 
    wId(m.id) ⊹ wCount(m.count) ⊹ wAtoms(m.atoms)
}



// vim: set ts=2 sw=2 et:
