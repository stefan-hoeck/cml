package cml

import cml.xml._, Xml._
import scalaz._, Scalaz._

/** About prefixes: 
  *
  * r: read from xml
  * w: write to xml
  * v: validate
  */
trait Attributes {
  import Attributes._

  // *** Atom Array ***
  def rAtoms: XmlReader[List[Atom]] = {
    def rAs = findElem(AtomArrayQn) >?> readElems[Atom](AtomQn)

    rAs ∘ { _ getOrElse Nil }
  }

  def wAtoms: XmlWriter[List[Atom]] = as ⇒ 
    if (as.isEmpty) ∅[ElemData]
    else writeElem(AtomArrayQn)(writeElems[Atom](AtomQn))(as)

  // *** Count ***
  def rCount: XmlReader[Option[Double]] =
    readAttr[Double](CountQn) validateO vCount

  def wCount: XmlWriter[Option[Double]] = writeAttrO(CountQn)

  def vCount(d: Double): ValRes[Double] = 
    if (d >= 0D) d.success
    else s"Count must be a non-negative number: $d".failureNel

  // *** Element Type ***
  def rElement: XmlReader[Element] = readMustHaveAttr(ElementTypeQn)
  def wElement: XmlWriter[Element] = writeAttr(ElementTypeQn)

  // *** Formal Charge ***
  def rFormalCharge: XmlReader[Option[Int]] = readAttr(FormalChargeQn)
  def wFormalCharge: XmlWriter[Option[Int]] = writeAttrO(FormalChargeQn)

  // *** Id ***
  def rId: XmlReader[String] = readMustHaveAttr[String](IdQn) validate vId
  def wId: XmlWriter[String] = writeAttr(IdQn)

  def vId(s: String): ValRes[String] = 
    if (s matches idR) s.success else s"Not a valid CML Id: $s".failureNel
}

object Attributes extends Attributes {
  private val idR = """[A-Za-z][A-Za-z0-9\.\-_]*"""

  final val AtomQn = cmlQn("atom")
  final val AtomArrayQn = cmlQn("atomArray")
  final val CountQn = cmlQn("count")
  final val ElementTypeQn = cmlQn("elementType")
  final val FormalChargeQn = cmlQn("formalCharge")
  final val IdQn = cmlQn("id")
  final val MoleculeQn = cmlQn("molecule")
}

// vim: set ts=2 sw=2 et:
