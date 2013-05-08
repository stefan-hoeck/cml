package cml

import Attributes._
import cml.xml._, Xml._
import scalaz._, Scalaz._

case class Molecule(
    id: String,
    count: Option[Double],
    atoms: List[Atom]) {

  def show: Cord = {
    def lines: Vector[Cord] = {
      val before: Vector[Cord] = 
        Vector(s"id = $id", s"count = $count", "Atoms:")

      val as = atoms map { a ⇒ Cord("  ", a.toString) } toVector

      val indented = (before ++ as) map { "  " +: _ }

      Cord("Molecule{") +: indented :+ Cord("}")
    }

    lines foldMap { _ :+ "\n" }
  }
}

object Molecule {
  implicit val MoleculeEqual = Equal.equalA[Molecule]

  implicit val MoleculeShow = Show show { m: Molecule ⇒ m.show }

  implicit val XmlReaderImpl: XmlReader[Molecule] =
    ^^(rId, rCount, rAtoms)(Molecule.apply)

  implicit val XmlWriterImpl: XmlWriter[Molecule] = m ⇒ 
    wId(m.id) ⊹ wCount(m.count) ⊹ wAtoms(m.atoms)
}



// vim: set ts=2 sw=2 et:
