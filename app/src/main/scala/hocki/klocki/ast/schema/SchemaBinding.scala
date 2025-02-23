package hocki.klocki.ast.schema

class SchemaBinding(val id: SchemaId):
  override def toString: String = s"${id}_@${hashCode()}"
