package hocki.klocki.semantics.graphs

import hocki.klocki.entities.DimSetVar
import hocki.klocki.names.NameGenerator

class BlockSchemaId

class BlockSchema
(
  val id: BlockSchemaId,
  val inVertices: List[DimSetVar],
  val outVertices: List[DimSetVar],
  val blocks: Set[Block],
  val edges: Set[(DimSetVar, DimSetVar)],
):
  def instantiate(using gen: NameGenerator): Block =
    Block(
      this.id,
      inVertices.map(_ -> gen.freshInDimSetVar()).toMap ++ outVertices.map(_ -> gen.freshOutDimSetVar()).toMap
    )
