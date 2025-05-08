package hocki.klocki.ast.vertex

import hocki.klocki.ast.Identifier

case class BlockId(name: String) extends Identifier:
  override def toString: String = name
