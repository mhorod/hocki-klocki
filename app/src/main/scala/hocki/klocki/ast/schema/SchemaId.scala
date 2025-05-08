package hocki.klocki.ast.schema

import hocki.klocki.ast.Identifier

case class SchemaId(name: String) extends Identifier:
  override def toString: String = name
