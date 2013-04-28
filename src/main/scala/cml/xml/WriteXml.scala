package cml.xml

import scala.xml.Node

trait WriteXml[-A] {
  def write(a: A): Seq[Node]
}

object WriteXml {
  def apply[A:WriteXml]: WriteXml[A] = implicitly
}

// vim: set ts=2 sw=2 et:
