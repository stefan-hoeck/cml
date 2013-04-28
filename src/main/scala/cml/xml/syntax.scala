package cml.xml

import cml.ValRes
import scala.xml.Node

object syntax {
  implicit class NodeSeqOps(val ns: Seq[Node]) extends AnyVal {
    def read[A:ReadXml]: ValRes[A] = ReadXml[A] read ns
  }

  implicit class AnyOps[A](val a: A) extends AnyVal {
    def xml(implicit X: WriteXml[A]): Seq[Node] = X write a
  }
}

// vim: set ts=2 sw=2 et:
