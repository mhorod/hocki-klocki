package hocki.klocki.ast

class DimBinding(id: DimId):
  override def toString: String = s"${id}_@${hashCode()}" 
