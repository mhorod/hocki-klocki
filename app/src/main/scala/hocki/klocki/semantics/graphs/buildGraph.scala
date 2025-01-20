package hocki.klocki.semantics.graphs

import hocki.klocki.analysis.ResolvedNames
import hocki.klocki.ast.Toplevel


def buildGraph(ast: Toplevel, nr: ResolvedNames): List[BlockSchema] =
  val schemaIds = nr.schemaNames.values.map(binding => binding -> BlockSchemaId()).toMap
  println(schemaIds)
  List()