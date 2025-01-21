package hocki.klocki.visualize

import hocki.klocki.entities.DimSetVar
import hocki.klocki.semantics.graphs.{BlockSchema, BlockSchemaId}


def unexpandedToGraphviz(schemata: List[BlockSchema]): String =
  val byId = schemata.map(schema => schema.id -> schema).toMap
  val str = schemata
    .filter(s => !s.name.contains("builtin"))
    .map(schema => unexpandedToGraphviz(schema, byId)).mkString("\n")
  s"""|digraph G {
      |  rankdir=TB
      |  $str
      |}""".stripMargin

def unexpandedToGraphviz(schema: BlockSchema, byId: Map[BlockSchemaId, BlockSchema]): String =
  val blocks = schema.blocks.map(
    block =>
      val blockSchema = byId(block.schemaId)
      val inVertices = blockSchema.inVertices.map(block.freshMapping)
      val outVertices = blockSchema.outVertices.map(block.freshMapping)

      val edges = (for
        inVertex <- inVertices
        outVertex <- outVertices
      yield s"${vertexName(schema, inVertex)} -> ${vertexName(schema, outVertex)} [style=invis]").mkString(";")

      val vertices = (inVertices ++ outVertices).map(v =>
        s"${vertexName(schema, v)} [label=${v.name}]"
      ).mkString(";")

      s"""|    subgraph cluster_${block.hashCode()} {
          |      label="${blockSchema.name}"
          |      $vertices
          |      $edges
          |    }""".stripMargin
  ).mkString("\n")

  val inVertices = schema.inVertices
  val outVertices = schema.outVertices
  val vertices = (inVertices ++ outVertices).map(v =>
    s"${vertexName(schema, v)} [label=${v.name}]"
  ).mkString(";")

  val edges = schema.edges.map(
    (from, to) =>
      s"${vertexName(schema, from)} -> ${vertexName(schema, to)}"
  ).mkString(";")
  s"""|  subgraph cluster_${schema.id.hashCode()} {
      |    label="${schema.name}"
      |    $vertices
      |    $blocks
      |    $edges
      |      |  }""".stripMargin

def vertexName(schema: BlockSchema, vertex: DimSetVar): String =
  s"V${schema.hashCode()}_${vertex.name}"