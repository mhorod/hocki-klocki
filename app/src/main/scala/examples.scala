package hocki.klocki

import names.{NameGenerator, SimpleNameGenerator}
import semantics.dims.{Dim, DimSetVar}
import semantics.graphs.BlockSchema
import typing.{addDimSchema, inferTypes, removeDimSchema, unionSchema}

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


def parallelDimensionRemovalExample =
  val (add_dim_a_schema, add_dim_a_ty) = removeDimSchema(Dim("a"))
  val (add_dim_b_schema, add_dim_b_ty) = removeDimSchema(Dim("b"))
  val (union_schema, union_ty) = unionSchema(2)

  val unionBlock1 = union_schema.instantiate
  val unionBlock2 = union_schema.instantiate
  val blockA = add_dim_a_schema.instantiate
  val blockB = add_dim_b_schema.instantiate

  val x0 = DimSetVar("X_0")
  val x1 = DimSetVar("X_1")
  val y = DimSetVar("Y")

  val v0 = unionBlock1.freshMapping(unionBlock1.schema.inVertices.head)
  val v1 = unionBlock1.freshMapping(unionBlock1.schema.inVertices(1))
  val v2 = unionBlock1.freshMapping(unionBlock1.schema.outVertices.head)

  val v3 = blockB.freshMapping(blockB.schema.inVertices.head)
  val v4 = blockB.freshMapping(blockB.schema.outVertices.head)

  val v5 = blockA.freshMapping(blockA.schema.inVertices.head)
  val v6 = blockA.freshMapping(blockA.schema.outVertices.head)

  val v7 = unionBlock2.freshMapping(unionBlock2.schema.inVertices.head)
  val v8 = unionBlock2.freshMapping(unionBlock2.schema.inVertices(1))
  val v9 = unionBlock2.freshMapping(unionBlock2.schema.outVertices.head)


  val schema = BlockSchema(
    List(x0, x1),
    List(y),
    Set(unionBlock1, unionBlock2, blockA, blockB),
    Set(
      x0 -> v0,
      x1 -> v1,
      x1 -> v3,
      v2 -> v5,
      v6 -> v7,
      v4 -> v8,
      v9 -> y
    )
  )

  val typing = collection.mutable.Map(
    add_dim_a_schema -> add_dim_a_ty,
    add_dim_b_schema -> add_dim_b_ty,
    union_schema -> union_ty
  )

  inferTypes(schema, typing)

def susExample =
  val dim_a = Dim("a")
  val (add_dim_a_schema, add_dim_a_ty) = addDimSchema(dim_a)
  val (remove_dim_a_schema, remove_dim_a_ty) = removeDimSchema(dim_a)

  val add_dim_a_block = add_dim_a_schema.instantiate
  val remove_dim_a_block = remove_dim_a_schema.instantiate

  val x = DimSetVar("X")
  val y0 = DimSetVar("Y_0")
  val y1 = DimSetVar("Y_1")

  val v0 = add_dim_a_block.freshMapping(add_dim_a_block.schema.inVertices.head)
  val v1 = add_dim_a_block.freshMapping(add_dim_a_block.schema.outVertices.head)

  val v2 = remove_dim_a_block.freshMapping(remove_dim_a_block.schema.inVertices.head)
  val v3 = remove_dim_a_block.freshMapping(remove_dim_a_block.schema.outVertices.head)

  val schema = BlockSchema(
    List(x),
    List(y0, y1),
    Set(add_dim_a_block, remove_dim_a_block),
    Set(
      x -> v0,
      v1 -> y0,
      x -> v2,
      v3 -> y1,
    )
  )

  val typing = collection.mutable.Map(
    add_dim_a_schema -> add_dim_a_ty,
    remove_dim_a_schema -> remove_dim_a_ty,
  )

  inferTypes(schema, typing)
