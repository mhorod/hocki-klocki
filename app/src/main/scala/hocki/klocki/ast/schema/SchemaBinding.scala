package hocki.klocki.ast.schema

import hocki.klocki.ast.Binding

class SchemaBinding(val id: SchemaId) extends Binding:
  override def toString: String = s"${id}_@${hashCode()}"
