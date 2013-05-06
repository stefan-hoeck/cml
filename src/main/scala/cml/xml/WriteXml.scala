package cml.xml

import scalaz.DList

trait WriteXml[-A] {
  def elemData(a: A): ElemData
}

object WriteXml extends WriteXmlFunctions {
  def apply[A:WriteXml]: WriteXml[A] = implicitly
}

trait WriteXmlFunctions {
  def writeData[A](f: A ⇒ ElemData): WriteXml[A] = new WriteXml[A] {
    def elemData(a: A): ElemData = f(a)
  }

  def writeAttr[A](f: A ⇒ String, qn: QName): WriteXml[A] =
    writeData(a ⇒ (DList(attr(qn, f(a))), DList()))

  def writeText[A](f: A ⇒ String, qn: QName): WriteXml[A] =
    writeElem(a ⇒ text(qn, f(a)))

  def writeElem[A](f: A ⇒ Elem): WriteXml[A] =
    writeData(a ⇒ (DList(), DList(f(a))))
}

trait WriteXmlInstances {
  // implicit def optionInstance[A:WriteXml]: WriteXml[Option[A]] =
}

// vim: set ts=2 sw=2 et:
