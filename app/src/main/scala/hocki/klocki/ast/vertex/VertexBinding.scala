package hocki.klocki.ast.vertex

import hocki.klocki.ast.vertex.{VertexBinding, VertexId}

sealed abstract class VertexBinding(val id: VertexId)

object VertexBinding:
  class Supplier(id: VertexId) extends VertexBinding(id):
    override def toString: String = s"${this.id}>_@${hashCode()}"

  class Consumer(id: VertexId) extends VertexBinding(id):
    override def toString: String = s">${this.id}_@${hashCode()}"
