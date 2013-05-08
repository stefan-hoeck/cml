
import scalaz._, Scalaz._
import cml.xml._

package object cml {
  type Errors = NonEmptyList[String]

  type ValRes[+A] = Validation[Errors,A]

  type DisRes[+A] = Errors \/ A

  final val CmlURI = "http://www.xml-cml.org/schema"

  final val ConventionURI = "http://www.xml-cml.org/convention/"

  def cmlQn(name: String) = qname(name, Some("cml"), Some(CmlURI))
}

// vim: set ts=2 sw=2 et:
