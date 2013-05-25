package cml

import cml.xml.Read
import scalaz._, Scalaz._

sealed abstract class BondOrder(val short: String, val symbol: String)

object BondOrder {
  case object Single extends BondOrder("S", "-")
  case object Double extends BondOrder("D", "=")
  case object Triple extends BondOrder("T", "#")
  case object Aromatic extends BondOrder("A", ":")
  case object Unknown extends BondOrder("unknown", "?")

  val values: List[BondOrder] =
    List(Single, Double, Triple, Aromatic, Unknown)

  private[this] val symbolMap: Map[String,BondOrder] =
    (values map (e ⇒ e.short.toLowerCase → e) toMap) ++
    Map("1" → Single, "2" → Double, "3" → Triple)

  def fromSymbol(s: String): Option[BondOrder] =
    symbolMap get s.toLowerCase

  def fromSymbolV(s: String): ValRes[BondOrder] = {
    def msg = s"Unknown bond order: $s"

    fromSymbol(s) toSuccess msg.wrapNel
  }

  implicit val BondOrderEqual: Equal[BondOrder] = Equal.equalA
  implicit val BondOrderRead = Read read fromSymbolV
  implicit val BondOrderShow = Show shows { e: BondOrder ⇒ e.short }
}

// vim: set ts=2 sw=2 et:
