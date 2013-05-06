package cml

import cml.xml.{ReadXml, WriteXml}
import language.implicitConversions
import scalaz._, Scalaz._

final case class CMLId(s: String) extends AnyVal

object CMLId {
  private final val pattern = """[A-Za-z][A-Za-z0-9\-_\.]*"""

  def read(s: String): ValRes[CMLId] =
    if (s matches pattern) CMLId(s).success
    else s"$s does not match id pattern".failureNel

  implicit def to(s: String) = CMLId(s)
  implicit def from(c: CMLId) = c.s
  implicit val WriteXmlImpl = WriteXml.singleAttr[CMLId](_.s, IdQn)
  implicit val ReadXmlImpl = ReadXml.fromAttr(read, IdQn)
}

// vim: set ts=2 sw=2 et:
