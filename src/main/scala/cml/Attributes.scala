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

  // *** AtomRefs2 ***
  private def aref2show = Show shows { a: AtomRefs2 ⇒ s"${a._1} ${a._2}" }
  private def aref2read = Read.read[AtomRefs2] { s ⇒
    s split " " toList match {
      case a::b::Nil ⇒  ^(vId(a), vId(b))(Pair.apply)
      case _         ⇒ s"Unknown format for AtomRefs2: $s".failureNel
    }
  }

  def rAtomRefs2: XmlReader[AtomRefs2] =
    readMustHaveAttr(AtomRefs2Qn)(aref2read)

  def wAtomRefs2: XmlWriter[AtomRefs2] =
    writeAttrS(AtomRefs2Qn)(aref2show)

  // *** Bond Array ***
  def rBonds: XmlReader[List[Bond]] = {
    def rBs = findElem(BondArrayQn) >?> readElems[Bond](BondQn)

    rBs ∘ { _ getOrElse Nil }
  }

  def wBonds: XmlWriter[List[Bond]] = bs ⇒ 
    if (bs.isEmpty) ∅[ElemData]
    else writeElem(BondArrayQn)(writeElems[Bond](BondQn))(bs)

  // *** Bond Order ***
  def rBondOrder: XmlReader[BondOrder] = readMustHaveAttr(BondOrderQn)
  def wBondOrder: XmlWriter[BondOrder] = writeAttrS(BondOrderQn)

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

  def rIdO: XmlReader[Option[String]] =
    readShouldHaveAttr[String](IdQn) validateO vId

  def wIdO: XmlWriter[Option[String]] = writeAttrO(IdQn)

  def vId(s: String): ValRes[String] = 
    if (s matches idR) s.success else s"Not a valid CML Id: $s".failureNel
}

object Attributes extends Attributes {
  private val idR = """[A-Za-z][A-Za-z0-9\.\-_]*"""

  final val AtomQn = cmlQn("atom")
  final val AtomArrayQn = cmlQn("atomArray")
  final val AtomRefs2Qn = cmlQn("atomRefs2")
  final val BondQn = cmlQn("bond")
  final val BondArrayQn = cmlQn("bondArray")
  final val BondOrderQn = cmlQn("order")
  final val CmlQn = cmlQn("cml")
  final val CountQn = cmlQn("count")
  final val ElementTypeQn = cmlQn("elementType")
  final val FormalChargeQn = cmlQn("formalCharge")
  final val IdQn = cmlQn("id")
  final val MoleculeQn = cmlQn("molecule")
}

// vim: set ts=2 sw=2 et:
