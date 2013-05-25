package cml

import cml.Attributes._
import cml.xml._, Xml._
import scales.utils._, ScalesUtils._, scales.xml._, ScalesXml._
import scales.xml.jaxen._
import scalaz._, Scalaz._

package object example {
  //loads xml document from an InputStream
  val doc = loadXml(getClass.getResourceAsStream("example.xml"))

  //the root element of the XML tree
  val root = loadXml(getClass.getResourceAsStream("example.xml")).rootElem

  /** Reading into a Molecule object **/

  //XmlReader is a type alias for a function from an xml (sub-tree) to
  //a pair containing a list of logging messages and a validated instance
  //of the parsed object. XmlReaders are highly composable as they have
  //an Applicative Functor and an Arrow instance. Most of the functionality
  //of XmlReaders can be found in the cml.xml.Xml file.
  //
  //`getElem` looks for a certain xml element and returns a failure if
  //none is found, the we take the element and try to read a molecule
  //from it.
  val reader: XmlReader[Molecule] =
    getElem(MoleculeQn) >=> XmlReader[Molecule]

  //Apply the root element of the parsed document to the XmlReader.
  //ReaderPair[A] is a type alias for
  //(DList[String],Validation[NonEmptyList[String],A])
  //The DList contains all loggings. Right now this is
  //just Strings without log levels, but adding those is trivial.
  //The Validation is either a Failure containing a (non-empty) list of
  //error messages in the case one or several things went wrong,
  //or a Success holding the parsed Molecule.
  //
  //Most of these type aliases are defined in the cml.xml package object.
  val mol: ReaderPair[Molecule] = reader(root)

  //Pretty prints the molecule including all logging messages
  val molPretty: String = showPair(mol)



  /** Parsing using Scales XML's DSL **/

  //The code below operates directly on the XML tree. This is unmodified
  //Scales XML syntax, so no input validation, no error handling, and no
  //logging.

  //Extracts all molecule elements from the root element
  val molPath = top(root) \* MoleculeQn

  //Extracts the string value of the molecules id attribute
  //If the attribute were prefixed with 'cml', we could use a different,
  //saver method: molPath *@ IdQn
  //I asked on the scales xml google group whether this should not also
  //be possible if a default namespace was defined as is the case in the
  //given file.
  //Note that for elements it works out of the box no matter whether they
  //are prefixed or not
  val molId: String = string(molPath *:@ IdQn.local)

  //Extracts all atom elements from the molecule
  val atoms = molPath \* AtomArrayQn \* AtomQn

  //Extracts the element types of all atoms in the molecule
  //Again we have to use the slightli more cumbersome
  //unprefixed version of the syntax
  val elementTypes: List[String] =
    (atoms *:@ ElementTypeQn.local).toList map { string(_) }


  /** Queriyng using XPaths directly **/
  
  //We can use XPath Strings directly at the cost of type safety
  val xpath = ScalesXPath("/cml/molecule/atomArray/atom/@elementType")
                .withNameConversion(ScalesXPath.localOnly)

  val elementTypes2: List[String] =
    xpath attributePaths top(root) map { string(_) } toList
}

// vim: set ts=2 sw=2 et:
