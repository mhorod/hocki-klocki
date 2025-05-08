package hocki.klocki.ast.dim

import hocki.klocki.ast.Identifier

case class DimId(name: String) extends Identifier:
  override def toString: String = name
