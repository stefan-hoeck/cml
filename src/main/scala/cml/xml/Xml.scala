package cml.xml

import cml.ValRes
import scalaz._, Scalaz._

object Xml 
  extends ReadFunctions
  with ReadInstances 
  with ReaderInstances
  with XmlFunctions {

  implicit class AnyOps[A](val a: A) extends AnyVal {
    def xml(qn: QName)(implicit F: XmlWriter[A]): Elem = elem(qn, F(a))
  }

  implicit class ElemOps(val v: Elem) extends AnyVal {
    def readV[A](implicit F: XmlReader[A]): ValRes[A] = F(v)._2

    def read[A](implicit F: XmlReader[A]): ReaderPair[A] = F(v)
  }

  implicit class ReaderOps[A,B](val r: Reader[A,B]) extends AnyVal {
    def bulkReader[F[_]:Traverse]: Reader[F[A],F[B]] =
      reader.bulkReader(r)

    def log(l: Log): Reader[A,B] = >=>(reader log l)

    def logs(ls: Logs): Reader[A,B] = >=>(reader logs ls)

    def validate[C](f: B ⇒ ValRes[C]): Reader[A,C] = >=>(reader liftV f)

    def >=>[C](that: Reader[B,C]): Reader[A,C] = reader.compose(that)(r)
  }

  implicit class ReaderOOps[A,B](val r: Reader[A,Option[B]]) extends AnyVal {

    def validateO[C](f: B ⇒ ValRes[C]): Reader[A,Option[C]] = 
      >?>(reader liftV f)

    def >?>[C](that: Reader[B,C]): Reader[A,Option[C]] =
      reader.composeO(that)(r)

    def >??>[C](that: Reader[B,Option[C]]): Reader[A,Option[C]] =
      reader.composeOO(that)(r)
  }
}

trait XmlFunctions {
  import Xml.{ReaderOps, ReaderOOps, ReaderApplicative}

  def findAttr(qn: QName): XmlReader[Option[String]] = e ⇒ 
    (reader.noLogs, byQn(qn)(e){ _ findAttrBy _ } map fromList success)

  def readAttr[A:Read](qn: QName): XmlReader[Option[A]] =
    findAttr(qn) validate Read[A].readO

  def readMustHaveAttr[A:Read](qn: QName): XmlReader[A] =
    readAttr[A](qn) >=> mustHaveAttr(qn)

  def readShouldHaveAttr[A:Read](qn: QName): XmlReader[Option[A]] =
    readAttr[A](qn) >=> shouldHaveAttr(qn)

  def findElem(qn: QName): XmlReader[Option[Elem]] = e ⇒ 
    (reader.noLogs, byQn(qn)(e){ _ filterElementQname _ } success)

  def findElems(qn: QName): XmlReader[List[Elem]] = e ⇒ 
    (reader.noLogs, byQns(qn)(e){ _ filterChildrenQname _ } success)

  def findText(qn: QName): XmlReader[Option[String]] =
    findElem(qn) ∘ { _ map { e ⇒ fromList(e.strContent) } }

  def readElem[A](qn: QName)(implicit F: XmlReader[A]): XmlReader[Option[A]] =
    findElem(qn) >?> F

  def readElems[A](qn: QName)(implicit F: XmlReader[A]): XmlReader[List[A]] =
    findElems(qn) >=> F.bulkReader

  def writeAttr[A](qn: QName): XmlWriter[A] =
    a ⇒ (DList(attr(qn, a.toString)), DList())

  def writeAttrS[A:Show](qn: QName): XmlWriter[A] =
    a ⇒ (DList(attr(qn, a.shows)), DList())

  def writeAttrO[A](qn: QName): XmlWriter[Option[A]] =
    _ foldMap writeAttr[A](qn)

  def writeElem[A](qn: QName)(implicit F: XmlWriter[A]): XmlWriter[A] =
    a ⇒ (DList(),DList(elem(qn, F(a))))

  def writeElems[A](qn: QName)(implicit F: XmlWriter[A]): XmlWriter[List[A]] =
    _ foldMap writeElem[A](qn)

  def writeText[A](qn: QName): XmlWriter[A] = 
    a ⇒ (DList(), DList(text(qn, a.toString)))

  def writeXml[A](a: A, qn: QName)(implicit F: XmlWriter[A]): Elem =
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

  private def shouldHave[A](qn: QName, name: String)
    : Reader[Option[A],Option[A]] = {
      def msg = s"Warning: $name ${qn.sname} was not found."

      _ match {
        case x@Some(_) ⇒ (reader.noLogs, x.success)
        case _         ⇒ (DList(msg), none.success)
      }
    }

  private def mustHave[A](qn: QName, name: String): Reader[Option[A],A] = {
    def msg = s"$name ${qn.sname} was not found."

    oa ⇒ (reader.noLogs, oa toSuccess msg.wrapNel)
  }

  def mustHaveElem[A](qn: QName) = mustHave[A](qn, "Element")
  def mustHaveAttr[A](qn: QName) = mustHave[A](qn, "Attribute")
  def shouldHaveElem[A](qn: QName) = shouldHave[A](qn, "Element")
  def shouldHaveAttr[A](qn: QName) = shouldHave[A](qn, "Attribute")
}

// vim: set ts=2 sw=2 et:
