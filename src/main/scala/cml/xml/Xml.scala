package cml.xml

import cml.ValRes
import scalaz._, Scalaz._

object Xml 
  extends WriteXmlFunctions
  with ReadXmlFunctions 
  with ReadInstances {

  //*** Implicit syntax classes ***/

  implicit class ElemOps(val e: Elem) extends AnyVal {
    def read[A:ReadXml]: ValRes[A] = ReadXml[A] read e

    def readAttr[A:Read](qn: QName): ValRes[A] = Read[A] readAttr (qn, e)

    def readElem[A:Read](qn: QName): ValRes[A] = Read[A] readElem (qn, e)

    def readAttrO[A:Read](qn: QName): ValRes[Option[A]] =
      Read[A] readAttrO (qn, e)

    def readElemO[A:Read](qn: QName): ValRes[Option[A]] =
      Read[A] readElemO (qn, e)
  }

  implicit class AnyOps[A](val a: A) extends AnyVal {
    def xml(qn: QName)(implicit X: WriteXml[A]): Elem = 
      elem(qn, X elemData a)

    def elemData(implicit X: WriteXml[A]): ElemData = 
      X elemData a

    def writeAttr(qn: QName)(implicit S: Show[A]): ElemData =
      (DList(attr(qn, S shows a)), DList())

    def writeAttrS(qn: QName): ElemData =
      (DList(attr(qn, a.toString)), DList())

    def writeElem(qn: QName)(implicit S: Show[A]): ElemData =
      (DList(), DList(text(qn, S shows a)))

    def writeElemS(qn: QName): ElemData =
      (DList(), DList(text(qn, a.toString)))
  }
}

// vim: set ts=2 sw=2 et:
