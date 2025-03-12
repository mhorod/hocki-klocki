package hocki.klocki.ast.schema

import hocki.klocki.ast.vertex.VertexBinding

sealed abstract class IfaceBinding(val suppliers: List[VertexBinding.Supplier], val consumers: List[VertexBinding.Consumer]):
  final def suppliersToString: String = suppliers.mkString(", ")

  final def consumersToString: String = consumers.mkString(", ")

  def allVerticesInOrder: List[VertexBinding]

object IfaceBinding:
  class Internal(suppliers: List[VertexBinding.Supplier], consumers: List[VertexBinding.Consumer]) extends IfaceBinding(suppliers, consumers):
    override def allVerticesInOrder: List[VertexBinding] = this.suppliers ++ this.consumers

    override def toString: String = s"[$suppliersToString | $consumersToString]"

  class External(consumers: List[VertexBinding.Consumer], suppliers: List[VertexBinding.Supplier]) extends IfaceBinding(suppliers, consumers):
    override def allVerticesInOrder: List[VertexBinding] = this.consumers ++ this.suppliers

    override def toString: String = s"[$consumersToString | $suppliersToString]"