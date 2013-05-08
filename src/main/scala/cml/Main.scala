package cml

import cml.xml._, Xml._
import scalaz._, Scalaz._, effect.{IO, SafeApp}

object Main extends SafeApp {
  override def runc = 
    IO.putStrLn(xmlString) >>
    IO.putStrLn("") >>
    IO.putStrLn(readResult.toString)

  import Element.{C,O}

  lazy val ethanol = Molecule("m1", None, List(
    Atom("c1", C, None),
    Atom("c2", C, None),
    Atom("o1", O, None)
  ))

  lazy val xmlString =
    prettyPrint(ethanol xml Attributes.MoleculeQn)

  lazy val readResult =
    parseString(xmlString) traverse { _.read[Molecule] }
}

// vim: set ts=2 sw=2 et:
