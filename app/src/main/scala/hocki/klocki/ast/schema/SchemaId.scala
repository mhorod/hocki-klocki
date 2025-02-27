package hocki.klocki.ast.schema

case class SchemaId(name: String):
  override def toString: String = name
