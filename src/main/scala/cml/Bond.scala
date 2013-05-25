package cml

import Attributes._
import cml.xml._, Xml._
import scalaz._, Scalaz._

case class Bond(
    id: Option[String],
    refs: AtomRefs2,
    order: BondOrder) {

  override def toString = {
    val idS = id.cata(s ⇒ s"; $s", "")

    s"${refs._1} ${order.symbol} ${refs._2}$idS"
  }
}

object Bond {
  implicit val BondEqual = Equal.equalA[Bond]

  implicit val BondShow = Show shows { a: Bond ⇒ a.toString }

  implicit val XmlReaderImpl: XmlReader[Bond] = 
    ^^(rIdO, rAtomRefs2, rBondOrder)(Bond.apply)

  implicit val XmlWriterImpl: XmlWriter[Bond] = b ⇒ 
    wIdO(b.id) ⊹ wAtomRefs2(b.refs) ⊹ wBondOrder(b.order)
}

// vim: set ts=2 sw=2 et:
