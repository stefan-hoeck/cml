package cml.xml

import cml.ValRes

trait AsAttr[A] {
  def tag: String

  def read(s: String): ValRes[A]

  def write(a: A): String
}

object AsAttr {
  def apply[A:AsAttr]: AsAttr[A] = implicitly
}

// vim: set ts=2 sw=2 et:
