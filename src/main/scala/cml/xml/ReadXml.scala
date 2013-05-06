package cml.xml

import cml.ValRes
import scalaz._, Scalaz._

trait ReadXml[+A] {
  def read(e: Elem): ValRes[A]
}

object ReadXml extends ReadXmlFunctions {
  def apply[A:ReadXml]: ReadXml[A] = implicitly
}

trait ReadXmlFunctions {
  def fromElem[A](f: Elem ⇒ ValRes[A]): ReadXml[A] = new ReadXml[A] {
    def read(e: Elem) = f(e)
  }

  def fromAttr[A](f: String ⇒ ValRes[A], qn: QName): ReadXml[A] =
    fromElem { attrValueGet(qn, _) flatMap f }

  def fromAttrO[A](f: Option[String] ⇒ ValRes[A], qn: QName): ReadXml[A] =
    fromElem { e ⇒ f(attrValue(qn, e)) }

  def fromText[A](f: String ⇒ ValRes[A], qn: QName): ReadXml[A] =
    fromElem { elemTextGet(qn, _) flatMap f }

  def fromTextO[A](f: Option[String] ⇒ ValRes[A], qn: QName): ReadXml[A] =
    fromElem { e ⇒ f(elemText(qn, e)) }
}

// vim: set ts=2 sw=2 et:
