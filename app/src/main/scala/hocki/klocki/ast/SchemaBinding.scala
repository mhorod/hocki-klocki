package hocki.klocki.ast

class SchemaBinding(val id: SchemaId):
  override def toString: String = s"${id}_@${hashCode()}"
