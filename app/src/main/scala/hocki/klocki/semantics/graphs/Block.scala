package hocki.klocki.semantics.graphs

import hocki.klocki.entities.DimSetVar

class Block
(
  val schemaId: BlockSchemaId,
  val freshMapping: Map[DimSetVar, DimSetVar]
)
