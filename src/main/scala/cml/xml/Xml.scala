package cml.xml

import cml.ValRes
import scalaz._, Scalaz._

object Xml 
  extends ReadFunctions
  with ReadInstances 
  with XmlFunctions
  with XmlInstances {

  implicit class AnyOps[A](val a: A) extends AnyVal {
    def xml(qn: QName)(implicit F: WriteXml[A]): Elem = elem(qn, F(a))
  }

  implicit class ElemOps(val v: Elem) extends AnyVal {
    def read[A](implicit F: ReadXml[A]): ValRes[A] = F(v)
  }
}

trait XmlInstances {
  implicit val ReadXmlApplicative: Applicative[ReadXml] =
    Applicative[({type λ[α] = Elem ⇒ α})#λ].compose[ValRes]
}

trait XmlFunctions {
  def attrValueO(qname: QName): ReadXml[Option[String]] =
    byQn(qname)(_){ _ findAttrBy _ } map fromList success

  def attrValue(qname: QName): ReadXml[String] =
    reval(attrValueO(qname)){ _ toSuccess s"Attribute missing: $qname".wrapNel }

  def findElemO(qname: QName): ReadXml[Option[Elem]] =
    byQn(qname)(_){ _ filterElementQname _ } success

  def findElem(qname: QName): ReadXml[Elem] =
    reval(findElemO(qname)){  _ toSuccess s"Element missing: $qname".wrapNel }

  def findElems(qname: QName): ReadXml[List[Elem]] =
    byQns(qname)(_){ _ filterChildrenQname _ } success

  def elemTextO(qname: QName): ReadXml[Option[String]] =
    readMapO(findElemO(qname)){ e ⇒ fromList(e.strContent) }

  def elemText(qname: QName): ReadXml[String] =
    readMap(findElem(qname)){ e ⇒ fromList(e.strContent) }

  def readElem[A](qname: QName)(implicit F: ReadXml[A]): ReadXml[A] =
    reval(findElem(qname))(F)

  def readElemO[A](qname: QName)(implicit F: ReadXml[A]): ReadXml[Option[A]] =
    revalO(findElemO(qname))(F)

  def readElems[A](qname: QName)(implicit F: ReadXml[A]): ReadXml[List[A]] =
    reval(findElems(qname)){ _ traverse F }

  def reval[A,B](ra: ReadXml[A])(v: A ⇒ ValRes[B]): ReadXml[B] =
    ra(_) flatMap v

  def revalO[A,B](ra: ReadXml[Option[A]])(v: A ⇒ ValRes[B])
    : ReadXml[Option[B]] = ra(_) flatMap { _ traverse v }

  def readMap[A,B](ra: ReadXml[A])(f: A ⇒ B): ReadXml[B] = ra(_) map f

  def readMapO[A,B](ra: ReadXml[Option[A]])(f: A ⇒ B): ReadXml[Option[B]] =
    ra(_) map { _ map f }

  def writeAttr[A](qn: QName): WriteXml[A] =
    a ⇒ (DList(attr(qn, a.toString)), DList())

  def writeAttrS[A:Show](qn: QName): WriteXml[A] =
    a ⇒ (DList(attr(qn, a.shows)), DList())

  def writeAttrO[A](qn: QName): WriteXml[Option[A]] =
    _ foldMap writeAttr[A](qn)

  def writeElem[A](qn: QName)(implicit F: WriteXml[A]): WriteXml[A] =
    a ⇒ (DList(),DList(elem(qn, F(a))))

  def writeElems[A](qn: QName)(implicit F: WriteXml[A]): WriteXml[List[A]] =
    _ foldMap writeElem[A](qn)

  def writeText[A](qn: QName): WriteXml[A] = 
    a ⇒ (DList(), DList(text(qn, a.toString)))

  def writeXml[A](a: A, qn: QName)(implicit F: WriteXml[A]): Elem =
    elem(qn, F(a))
    

  private def byQn[A,B](qn: QName)(a: A)(f: (A, (QName ⇒ Boolean)) ⇒ Option[B])
    : Option[B] = {
      def name(qn1: QName) = qn.name == qn1.name
      def prefName(qn1: QName) = (qn.name == qn1.name) &&
                                 (qn.prefix == qn1.prefix)

      f(a, prefName) orElse f(a, name)
    }

  private def byQns[A,B](qn: QName)(a: A)(f: (A, (QName ⇒ Boolean)) ⇒ List[B])
    : List[B] = {
      def name(qn1: QName) = qn.name == qn1.name
      def prefName(qn1: QName) = (qn.name == qn1.name) &&
                                 (qn.prefix == qn1.prefix)

      val withPrefix = f(a, prefName) 

      if (withPrefix.isEmpty) f(a, name) else withPrefix
    }
}

// vim: set ts=2 sw=2 et:
