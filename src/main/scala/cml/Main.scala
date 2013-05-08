package cml

import cml.xml._, Xml._
import scalaz._, Scalaz._, effect.{IO, SafeApp}

object Main extends SafeApp {
  override def runc = 
    IO.putStrLn(xmlString) >>
    IO.putStrLn("") >>
    IO.putStrLn(readResult) >>
    IO.putStrLn("") >>
    IO.putStrLn(readResultInvalid)

  import Element.{C,O}

  lazy val ethanol = Molecule("m1", None, List(
    Atom("c1", C, None),
    Atom("c2", C, None),
    Atom("o1", O, None)
  ))

  lazy val xmlString =
    prettyPrint(ethanol xml Attributes.MoleculeQn)

  lazy val readResult = parseAndShow[Molecule](xmlString)

  lazy val readResultInvalid = parseAndShow[Molecule](invalid)

  lazy val invalid = """
    <cml:molecule cml:id="$$">
      <cml:atomArray>
        <cml:atom cml:id="221" cml:elementType="boo"/>
        <cml:atom cml:id="c2" cml:formalCharge="boo" cml:elementType="C"/>
        <cml:atom cml:elementType="O"/>
      </cml:atomArray>
    </cml:molecule>"""
                    
}

// vim: set ts=2 sw=2 et:
