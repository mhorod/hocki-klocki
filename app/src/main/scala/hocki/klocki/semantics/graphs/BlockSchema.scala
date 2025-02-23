package hocki.klocki.semantics.graphs

import hocki.klocki.entities.{Dim, DimSetVar}

class BlockSchemaId

class BlockSchema
(
  val id: BlockSchemaId,
  val name: String,
  val universalDims: List[Dim],
  val existentialDims: List[Dim],
  val inVertices: List[DimSetVar],
  val outVertices: List[DimSetVar],
  val blocks: Set[Block],
  val edges: Set[(DimSetVar, DimSetVar)],
)

def schemaToString(schema: BlockSchema): String =
  val inVertices = schema.inVertices.map(_.name).mkString(", ")
  val outVertices = schema.outVertices.map(_.name).mkString(", ")
  val blocks = if schema.blocks.isEmpty then "" else "\n  " + schema.blocks.map(blockToString).mkString("\n  ") + "\n"
  val edges = schema.edges.map((from, to) => s"$from -> $to").mkString(", ")
  s"BlockSchema(${schema.id}, ${schema.name}, [$inVertices | $outVertices] $blocks link $edges)"