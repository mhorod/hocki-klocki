package hocki.klocki.semantics.graphs

import hocki.klocki.entities.DimSetVar
import hocki.klocki.names.NameGenerator

class BlockSchemaId

class BlockSchema
(
  val id: BlockSchemaId,
  val name: String,
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

def schemaToString(schema: BlockSchema): String =
  val inVertices = schema.inVertices.map(_.name).mkString(", ")
  val outVertices = schema.outVertices.map(_.name).mkString(", ")
  val blocks = if schema.blocks.isEmpty then "" else "\n  " + schema.blocks.map(blockToString).mkString("\n  ") + "\n"
  val edges = schema.edges.map((from, to) => s"$from -> $to").mkString(", ")
  s"BlockSchema(${schema.id}, ${schema.name}, [$inVertices | $outVertices] $blocks link $edges)"