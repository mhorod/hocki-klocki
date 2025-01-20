package hocki.klocki.semantics.graphs

import hocki.klocki.entities.DimSetVar

class Block
(
  val schemaId: BlockSchemaId,
  val freshMapping: Map[DimSetVar, DimSetVar]
)

def blockToString(block: Block): String =
  val freshMapping = block.freshMapping.map((from, to) => s"$from -> $to").mkString(", ")
  s"Block(${block.schemaId}, $freshMapping)"
