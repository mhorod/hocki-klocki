package hocki.klocki.ast.vertex

import hocki.klocki.ast.Identifier

case class VertexId(name: String) extends Identifier:
  override def toString: String = name
