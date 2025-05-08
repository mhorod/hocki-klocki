package hocki.klocki.ast.dim

import hocki.klocki.ast.Binding

import hocki.klocki.ast.dim.DimId

class DimBinding(val id: DimId) extends Binding:
  override def toString: String = s"${id}_@${hashCode()}" 
