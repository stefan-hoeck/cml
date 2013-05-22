package cml

import scalaz._, Scalaz._
import scales.utils._, ScalesUtils._, scales.xml._, ScalesXml._

/** Defines type aliases and basic functions for interacting with an
  * XML backend.
  *
  * Comments S. Höck:
  * The functions and aliases in this package object are all that have
  * to be rewritten when changing the XML-backend.
  */
package object xml {
  type Namespace = scales.xml.PrefixedNamespace

  type QName = scales.xml.PrefixedQName

  type Attr = Attribute

  type Elem = scales.xml.XmlTree

  type ElemData = (DList[Attr], DList[Elem])

  type Log = String

  type Logs = DList[String]

  type ReaderPair[+A] = (Logs,ValRes[A])

  type Reader[-A,+B] = A ⇒ ReaderPair[B]

  type XmlReader[+A] = Reader[Elem,A]

  type XmlWriter[-A] = A ⇒ ElemData

  def ns(pre: String, uri: String): Namespace = 
    scales.xml.Namespace(uri) prefixed pre

  def qname(name: String, ns: Namespace): QName = ns(name)

  def attr(qname: QName, value: String): Attr = qname → value

  def text(qname: QName, txt: String): Elem = qname / txt toTree

  def elem(qname: QName, data: ElemData): Elem =
    qname /@ (data._1.toList: _*) / (data._2.toList: _*) toTree

  def prettyPrint(elem: Elem): String = asString(elem)

  def parseString(s: String): Elem =
    top(loadXml(new java.io.StringReader(s))).tree

  def parseAndShow[A:XmlReader:Show](s: String): String = {
    import Xml.ElemOps

    showPair(parseString(s).read[A])
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
}

// vim: set ts=2 sw=2 et:
