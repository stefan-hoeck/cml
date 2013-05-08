package cml

import scalaz._, Scalaz._
import scalaz.xml.{CData, CDataKind, Element ⇒ ElemZ,
                   Attr ⇒ AttrZ, QName ⇒ QNameZ, Content}
import scalaz.xml.Xml._

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

  type Log = String

  type Logs = DList[String]

  type ReaderPair[+A] = (Logs,ValRes[A])

  type Reader[-A,+B] = A ⇒ ReaderPair[B]

  type XmlReader[+A] = Reader[Elem,A]

  type XmlWriter[-A] = A ⇒ ElemData

  def qname(name: String, pre: Option[String], uri: Option[String]): QName =
    QNameZ qname (name.toList, uri map toList, pre map toList)

  def attr(qname: QName, value: String): Attr =
    AttrZ attr (qname, value.toList)

  def text(qname: QName, txt: String): Elem =
    ElemZ element (qname, Nil, List(
      Content text CData.cdata(CDataKind.cdataText, txt.toList)))

  def elem(qname: QName, data: ElemData): Elem =
    ElemZ element (qname, data._1.toList, data._2 map Content.elem toList)

  def prettyPrint(elem: Elem): String = elem sxprints pretty

  def parseString(s: String): Option[Elem] =
    s.parseXml flatMap { _.elem } headOption

  def parseAndShow[A:XmlReader:Show](s: String): String = {
    import Xml.ElemOps

    parseString(s) map { e ⇒ showPair(e.read[A]) } getOrElse ""
  }

  def showPair[A:Show](p: ReaderPair[A]): String = p match {
    case (logs,va) ⇒ {
      def logS = logs.toList mkString "  \n"
      def errors(es: NonEmptyList[String]): String = {
        val erS = es.list mkString "  \n"

        s"The following errors occured during XML parsing:\n$erS"
      }

      def aString = va fold (errors, _.shows)

      s"Logs:\n$logS\n\n$aString"
    }
  }

  private[xml] def toList(s: String) = s.toList
  private[xml] def fromList(cs: List[Char]) =  cs mkString ""
}

// vim: set ts=2 sw=2 et:
