package cml.xml

import cml.ValRes
import scala.xml.Node

trait ReadXml[+A] {
  def read(ns: Seq[Node]): ValRes[A]
}

object ReadXml {
  def apply[A:ReadXml]: ReadXml[A] = implicitly
}

// vim: set ts=2 sw=2 et:
