package hocki.klocki.typing

import hocki.klocki.ast.Abstra
import hocki.klocki.ast.schema.SchemaExpr

private def getMonomorphic(impl: Abstra): Abstra.OnIface = impl match
  case onIface: Abstra.OnIface => onIface
  case _ => throw IllegalStateException("Rank 1+ abstractions verboten")

private def getLeaf(expr: SchemaExpr): SchemaExpr.Leaf = expr match
  case leaf: SchemaExpr.Leaf => leaf
  case _ => throw IllegalStateException("Rank 1+ expressions verboten")