package hocki.klocki.ast

sealed abstract class VertexBinding(val id: VertexId)

object VertexBinding:
  class Supplier(id: VertexId) extends VertexBinding(id):
    override def toString: String = s"${this.id}>_@${hashCode()}"

  class Consumer(id: VertexId) extends VertexBinding(id):
    override def toString: String = s">${this.id}_@${hashCode()}"
