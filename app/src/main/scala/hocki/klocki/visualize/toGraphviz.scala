package hocki.klocki.visualize

import hocki.klocki.entities.DimSetVar
import hocki.klocki.semantics.graphs.{BlockSchema, BlockSchemaId}


def schemataToGraphviz(schemata: List[BlockSchema], expansionDepth: Int): String =
  println(s"Expanding schemata to graphviz, $expansionDepth")
  val byId = schemata.map(schema => schema.id -> schema).toMap
  val str = schemata
    .filter(s => !s.name.contains("builtin"))
    .map(schema => schemaToGraphviz(schema, byId, expansionDepth, Map(), s"${schema.hashCode()}")).mkString("\n")
  s"""|digraph G {
      |  rankdir=TB
      |  $str
      |}""".stripMargin

def schemaToGraphviz
(
  schema: BlockSchema,
  byId: Map[BlockSchemaId, BlockSchema],
  expansionDepth: Int,
  interfaceMapping: Map[DimSetVar, String],
  vertexPrefix: String
): String =
  val nestedDepth = if expansionDepth > 0 then expansionDepth - 1 else expansionDepth

  def vertexName(v: DimSetVar): String =
    if interfaceMapping.contains(v) then
      interfaceMapping(v)
    else
      s"V${vertexPrefix}_${v.name}"

  val blocks =
    if expansionDepth == 0 then ""
    else
      schema.blocks.map(
        block =>
          val blockSchema = byId(block.schemaId)
          val mapping = block.freshMapping
          val interfaceMapping = (blockSchema.outVertices ++ blockSchema.inVertices)
            .map(v => v -> vertexName(mapping(v)))
            .toMap
          schemaToGraphviz(blockSchema, byId, nestedDepth, interfaceMapping, vertexPrefix + s"_${block.hashCode()}")
      ).mkString("\n")

  val inVertices = schema.inVertices
  val outVertices = schema.outVertices
  val vertices = (inVertices ++ outVertices).map(v =>
    s"${vertexName(v)} [label=${v.name}]"
  ).mkString(";")

  val edges =
    if expansionDepth == 0 || schema.edges.isEmpty then
      (for (from <- inVertices; to <- outVertices) yield
        s"${vertexName(from)} -> ${vertexName(to)} [style=invis]").mkString(";")
    else
      schema.edges.map(
        (from, to) =>
          s"${vertexName(from)} -> ${vertexName(to)}"
      ).mkString(";")

  s"""|  subgraph cluster_S$vertexPrefix {
      |    label="${schema.name}"
      |    $vertices
      |    $blocks
      |    $edges
      |      |  }""".stripMargin

def vertexName(schema: BlockSchema, vertex: DimSetVar): String =
  s"V${schema.hashCode()}_${vertex.name}"