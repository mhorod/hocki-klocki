package hocki.klocki.visualize

import hocki.klocki.semantics.graphs.{BlockSchema, BlockSchemaId}


def unexpandedToGraphviz(schemata: List[BlockSchema]): String =
  val byId = schemata.map(schema => schema.id -> schema).toMap
  val str = schemata.map(schema => unexpandedToGraphviz(schema, byId)).mkString("\n")
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
      yield s"$inVertex -> $outVertex [style=invis]").mkString(";")

      val vertexNames = (inVertices ++ outVertices).map(_.name).mkString(";")
      s"""|    subgraph cluster_${block.hashCode()} {
          |      label="${blockSchema.name}"
          |      $vertexNames
          |      $edges
          |    }""".stripMargin
  ).mkString("\n")

  val edges = schema.edges.map((from, to) => s"$from -> $to").mkString(";")
  s"""|  subgraph cluster_${schema.id.hashCode()} {
      |    label="${schema.name}"
      |    $blocks
      |    $edges
      |  }""".stripMargin