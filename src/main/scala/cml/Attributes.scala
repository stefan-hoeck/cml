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
  def rAtoms: ReadXml[List[Atom]] = {
    def rArray = findElemO(AtomArrayQn)
    def rAs = revalO(rArray)(readElems[Atom](AtomQn))

    readMap(rAs)(_.toList.flatten)
  }

  def wAtoms: WriteXml[List[Atom]] = as ⇒ 
    if (as.isEmpty) ∅[ElemData]
    else writeElem(AtomArrayQn)(writeElems[Atom](AtomQn))(as)

  // *** Count ***
  def rCount: ReadXml[Option[Double]] =
    revalO(readAttrO[Double](CountQn))(vCount)

  def wCount: WriteXml[Option[Double]] = writeAttrO(CountQn)

  def vCount(d: Double): ValRes[Double] = 
    if (d >= 0D) d.success
    else s"Count must be a non-negative number: $d".failureNel

  // *** Element Type ***
  def rElement: ReadXml[Element] = readAttr(ElementTypeQn)
  def wElement: WriteXml[Element] = writeAttr(ElementTypeQn)

  // *** Formal Charge ***
  def rFormalCharge: ReadXml[Option[Int]] = readAttrO(FormalChargeQn)
  def wFormalCharge: WriteXml[Option[Int]] = writeAttrO(FormalChargeQn)

  // *** Id ***
  def rId: ReadXml[String] = reval(readAttr[String](IdQn))(vId)
  def wId: WriteXml[String] = writeAttr(IdQn)

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
