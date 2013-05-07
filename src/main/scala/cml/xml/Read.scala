package cml.xml

import cml.ValRes
import scalaz._, Scalaz._

/** A type class similar to the `Read` type class in Haskell but
  * which returns validated results after parsing a `String`.
  */
trait Read[+A] {
  def read(s: String): ValRes[A]
}

object Read extends ReadFunctions with ReadInstances {
  def apply[A:Read]: Read[A] = implicitly
}

trait ReadFunctions {
  import Xml._

  def read[A](f: String ⇒ ValRes[A]): Read[A] =
    new Read[A] { def read(s: String) = f(s) }

  def readAttr[A:Read](qn: QName): XmlReader[A] = ???
    //reval(attrValue(qn))(Read[A].read)

  def readAttrO[A:Read](qn: QName): XmlReader[Option[A]] = ???
    //revalO(attrValueO(qn))(Read[A].read)

  def readElem[A:Read](qn: QName): XmlReader[A] = ???
    //reval(elemText(qn))(Read[A].read)

  def readElemO[A:Read](qn: QName): XmlReader[Option[A]] = ???
    //revalO(elemTextO(qn))(Read[A].read)
}

trait ReadInstances {
  private def inst[A](
    v: String ⇒ Validation[Exception, A],
    msg: String ⇒ String
  ): Read[A] = new Read[A] {
    override def read(s: String) =
      ((_: Exception) ⇒ msg(s).wrapNel) <-: v(s.trim)
  }

  implicit val stringInst = Read.read[String](_.success)

  implicit val booleanInst =
    inst(_.parseBoolean, s ⇒ s"Not a boolean: $s")

  implicit val intInst =
    inst(_.parseInt, s ⇒ s"Not an integer: $s")
  
  implicit val longInst =
    inst(_.parseLong, s ⇒ s"Not an integer: $s") 

  implicit val doubleInst =
    inst(_.parseDouble, s ⇒ s"Not a floating point number: $s") 

  implicit val byteInst =
    inst(_.parseByte, s ⇒ s"Not an integer: $s") 

  implicit val shortInst =
    inst(_.parseShort, s ⇒ s"Not an integer: $s") 

  implicit val floatInst =
    inst(_.parseFloat, s ⇒ s"Not a floating point number: $s") 
}

// vim: set ts=2 sw=2 et:
