package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}
import hocki.klocki.semantics.graphs.{BlockSchema, builtinSchema}

def addDimSchema(dim: Dim): (BlockSchema, BlockTy) =
  val schema = builtinSchema(1, 1)
  val x = schema.inVertices.head
  val y = schema.outVertices.head
  val constraints = Set(
    dim notIn x,
    dim in y,
    dim dependsOn (x without Set(dim)),
    y inducedBy Set(x without Set(dim)),
  )
  (schema, BlockTy(constraints))

def removeDimSchema(dim: Dim): (BlockSchema, BlockTy) =
  val schema = builtinSchema(1, 1)
  val x = schema.inVertices.head
  val y = schema.outVertices.head
  val constraints = Set(
    dim notIn y,
    dim inUnion Set(x),
    y inducedBy Set(x without Set(dim))
  )
  (schema, BlockTy(constraints))

def unionSchema(inCount: Int): (BlockSchema, BlockTy) =
  val schema = builtinSchema(inCount, 1)
  val xs = schema.inVertices
  val y = schema.outVertices.head
  val constraints: Set[Constraint] = Set(y inducedBy xs.toSet.map(_ without Set()))
  (schema, BlockTy(constraints))
