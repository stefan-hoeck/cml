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
  val rAtoms: ReadXml[List[Atom]] = {
    def rArray = findElemO(AtomArrayQn)
    def rAs = revalO(rArray)(readElems[Atom](AtomQn))

    readMap(rAs)(_.toList.flatten)
  }

  val wAtoms: WriteXml[List[Atom]] =
    writeElems(AtomArrayQn)(writeElem(AtomQn))

  // *** Count ***
  val rCount: ReadXml[Option[Double]] = readAttrO(CountQn)
  val wCount: WriteXml[Option[Double]] = writeAttrO(ElementTypeQn)

  // *** Element Type ***
  val rElement: ReadXml[Element] = readAttr(ElementTypeQn)
  val wElement: WriteXml[Element] = writeAttr(ElementTypeQn)

  // *** Formal Charge ***
  val rFormalCharge: ReadXml[Option[Int]] = readAttrO(FormalChargeQn)
  val wFormalCharge: WriteXml[Option[Int]] = writeAttrO(FormalChargeQn)

  // *** Id ***
  val rId: ReadXml[String] = reval(readAttr[String](IdQn))(vId)
  val wId: WriteXml[String] = writeAttr(IdQn)

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
}

// vim: set ts=2 sw=2 et:
