package hocki.klocki

import names.{NameGenerator, SimpleNameGenerator}
import semantics.dims.{Dim, DimSetVar}
import semantics.graphs.{BlockSchema, builtinSchema}
import typing.{addDimSchema, inferTypes}

given NameGenerator = SimpleNameGenerator()

def chainedDimensionIntroductionsExample =
  val (add_dim_a_schema, add_dim_a_ty) = addDimSchema(Dim("a"))
  val (add_dim_b_schema, add_dim_b_ty) = addDimSchema(Dim("b"))
  val blockA = add_dim_a_schema.instantiate
  val blockB = add_dim_b_schema.instantiate

  val v0 = DimSetVar("X")
  val v1 = blockA.freshMapping(blockA.schema.inVertices.head)
  val v2 = blockA.freshMapping(blockA.schema.outVertices.head)
  val v3 = blockB.freshMapping(blockB.schema.inVertices.head)
  val v4 = blockB.freshMapping(blockB.schema.outVertices.head)
  val v5 = DimSetVar("Y")

  val schema = BlockSchema(
    List(v0),
    List(v5),
    Set(blockA, blockB),
    Set(v0 -> v1, v2 -> v3, v4 -> v5)
  )
  val typing = collection.mutable.Map(
    add_dim_a_schema -> add_dim_a_ty,
    add_dim_b_schema -> add_dim_b_ty
  )

  inferTypes(schema, typing)
