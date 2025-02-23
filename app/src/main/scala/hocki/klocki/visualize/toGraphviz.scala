package hocki.klocki.visualize

import hocki.klocki.entities.{Dim, DimSetVar}
import hocki.klocki.semantics.graphs.{BlockSchema, BlockSchemaId, blockToString, schemaToString}


def schemataToGraphviz(schemata: List[BlockSchema], expansionDepth: Int): String =
  println(s"Expanding schemata to graphviz, $expansionDepth")
  val byId = schemata.map(schema => schema.id -> schema).toMap
  val str = schemata
    .filter(s => !s.name.contains("builtin"))
    .map(schema => schemaToGraphviz(schema, byId, expansionDepth, Map(), Map(), s"${schema.hashCode()}")).mkString("\n")
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
  dimMapping: Map[Dim, Dim],
  vertexPrefix: String
): String =
  val nestedDepth = if expansionDepth > 0 then expansionDepth - 1 else expansionDepth

  def vertexName(v: DimSetVar): String = interfaceMapping.getOrElse(v, s"V${vertexPrefix}_${v.name}")

  def dimName(d: Dim): String = dimMapping.getOrElse(d, d).toString

  val blocks =
    if expansionDepth == 0 then ""
    else
      schema.blocks.map(
        block =>
          val blockSchema = byId(block.schemaId)
          val mapping = block.freshMapping
          println(schema.name)
          println(blockSchema.name)
          println(block.dimMapping)
          println(dimMapping)
          val interfaceMapping = (blockSchema.outVertices ++ blockSchema.inVertices)
            .map(v => v -> vertexName(mapping(v)))
            .toMap
          val newDimMapping = block.dimMapping.map((k, v) => k -> dimMapping.getOrElse(v, v))
          schemaToGraphviz(blockSchema, byId, nestedDepth, interfaceMapping, newDimMapping, vertexPrefix + s"_${block.hashCode()}")
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

  println(s"Schema: ${schema.name}, mapping: $dimMapping, ${schema.universalDims}, ${schema.existentialDims}")
  val universalDimArgs = schema.universalDims.map(dimName).mkString(", ")
  val existentialDimArgs = schema.existentialDims.map(dimName).mkString(", ")

  val dimArgs =
    if schema.name.contains("builtin") then
      universalDimArgs + existentialDimArgs
    else if universalDimArgs.nonEmpty || existentialDimArgs.nonEmpty then
      s"<$universalDimArgs | $existentialDimArgs>"
    else
      ""

  s"""|  subgraph cluster_S$vertexPrefix {
      |    label="${schema.name.replace("builtin ", "")}$dimArgs"
      |    labeljust="l"
      |    $vertices
      |    $blocks
      |    $edges
      |      |  }""".stripMargin

def vertexName(schema: BlockSchema, vertex: DimSetVar): String =
  s"V${schema.hashCode()}_${vertex.name}"