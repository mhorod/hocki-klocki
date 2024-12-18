package hocki.klocki.semantics.graphs

import hocki.klocki.names.NameGenerator
import hocki.klocki.semantics.dims.DimSetVar

class BlockSchema(
  val inVertices: List[DimSetVar],
  val outVertices: List[DimSetVar],
  val blocks: Set[Block],
  val edges: Set[(DimSetVar, DimSetVar)],
):
  def instantiate(using gen: NameGenerator): Block =
    Block(
      this,
      inVertices.map(_ -> gen.freshInDimSetVar()).toMap ++ outVertices.map(_ -> gen.freshOutDimSetVar()).toMap
    )
