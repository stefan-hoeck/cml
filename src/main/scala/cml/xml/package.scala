package cml

import scalaz._, Scalaz._
import scalaz.xml.{CData, CDataKind, Element ⇒ ElemZ,
                   Attr ⇒ AttrZ, QName ⇒ QNameZ, Content}
/** Defines type aliases and basic functions for interacting with an
  * XML backend.
  *
  * Comments S. Höck:
  * The functions and aliases in this package object are all that have
  * to be rewritten when changing the XML-backend.
  */
package object xml {
  type QName = QNameZ
  type Attr = AttrZ
  type Elem = ElemZ

  type ElemData = (DList[Attr], DList[Elem])

  def qname(name: String, pre: Option[String], uri: Option[String]): QName =
    QNameZ qname (name.toList, uri map toList, pre map toList)

  def attr(qname: QName, value: String): Attr =
    AttrZ attr (qname, value.toList)

  def text(qname: QName, txt: String): Elem =
    ElemZ element (qname, Nil, List(
      Content text CData.cdata(CDataKind.cdataText, txt.toList)))

  def elem(qname: QName, data: ElemData): Elem =
    ElemZ element (qname, data._1.toList, data._2 map Content.elem toList)

  def attrValue(qname: QName, elem: Elem): Option[String] =
    byQn(qname)(elem){ _ findAttrBy _ } map fromList

  def attrValueGet(qname: QName, elem: Elem): ValRes[String] =
    attrValue(qname, elem) toSuccess s"Attribute missing: $qname".wrapNel

  def elemText(qname: QName, elem: Elem): Option[String] =
    byQn(qname)(elem){ _ filterElementQname _ } map { e ⇒ fromList(e.strContent) }

  def elemTextGet(qname: QName, elem: Elem): ValRes[String] =
    elemText(qname, elem) toSuccess s"Element missing: $qname".wrapNel

  private def byQn[A,B](qn: QName)(a: A)(f: (A, (QName ⇒ Boolean)) ⇒ Option[B])
    : Option[B] = {
      def name(qn1: QName) = qn.name == qn1.name
      def prefName(qn1: QName) = (qn.name == qn1.name) &&
                                 (qn.prefix == qn1.prefix)

      f(a, prefName) orElse f(a, name)
    }

  private def toList(s: String) = s.toList
  private def fromList(cs: List[Char]) =  cs mkString ""
}

// vim: set ts=2 sw=2 et:
