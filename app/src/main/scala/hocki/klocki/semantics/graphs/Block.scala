package hocki.klocki.semantics.graphs

import hocki.klocki.entities.{Dim, DimSetVar}

class Block
(
  val schemaId: BlockSchemaId,
  val freshMapping: Map[DimSetVar, DimSetVar],
  val dimMapping: Map[Dim, Dim],
)

def blockToString(block: Block): String =
  val freshMapping = block.freshMapping.map((from, to) => s"$from -> $to").mkString(", ")
  s"Block(${block.schemaId}, $freshMapping)"
