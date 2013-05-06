package cml.xml

import scalaz.DList

trait WriteXml[-A] {
  def elemData(a: A): ElemData
}

object WriteXml extends WriteXmlFunctions {
  def apply[A:WriteXml]: WriteXml[A] = implicitly
}

trait WriteXmlFunctions {
  def elemData[A](f: A ⇒ ElemData): WriteXml[A] = new WriteXml[A] {
    def elemData(a: A): ElemData = f(a)
  }

  def singleAttr[A](f: A ⇒ String, qn: QName): WriteXml[A] =
    elemData(a ⇒ (DList(attr(qn, f(a))), DList()))

  def singleElem[A](f: A ⇒ String, qn: QName): WriteXml[A] =
    elemData(a ⇒ (DList(), DList(text(qn, f(a)))))
}

trait WriteXmlInstances {
  // implicit def optionInstance[A:WriteXml]: WriteXml[Option[A]] =
}

// vim: set ts=2 sw=2 et:
