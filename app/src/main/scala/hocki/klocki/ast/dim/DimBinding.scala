package hocki.klocki.ast.dim

import hocki.klocki.ast.dim.DimId

class DimBinding(val id: DimId):
  override def toString: String = s"${id}_@${hashCode()}" 
