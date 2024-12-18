package hocki.klocki
package semantics.graphs

import semantics.dims.DimSetVar

import names.NameGenerator

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
