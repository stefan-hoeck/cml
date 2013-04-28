
import scalaz._, Scalaz._
import scalaz.xml._

package object cml {
  type Errors = NonEmptyList[String]

  type ValRes[+A] = Validation[Errors,A]

  type DisRes[+A] = Errors \/ A

  final val CmlURI = "http://www.xml-cml.org/schema"

  final val ConventionURI = "http://www.xml-cml.org/convention/"

  final val CmlQN = QName qname ("cml".toList, CmlURI.toList.some)
}

// vim: set ts=2 sw=2 et:
