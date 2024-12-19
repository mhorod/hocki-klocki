import hocki.klocki.names.{NameGenerator, SimpleNameGenerator}
import hocki.klocki.semantics.dims.{Dim, DimSetVar}
import hocki.klocki.semantics.graphs.BlockSchema
import hocki.klocki.typing.{BlockTy, addDimSchema, inferTypes, removeDimSchema, unionSchema}

import scala.collection.mutable

given NameGenerator = SimpleNameGenerator()

@main
def chainedDimensionIntroductionsExample: BlockTy =
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

@main
def parallelDimensionRemovalExample: BlockTy =
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

@main
def susExample: BlockTy =
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

def funnySchema: (BlockSchema, mutable.Map[BlockSchema, BlockTy]) =
  val dim_a = Dim("a")
  val dim_b = Dim("b")

  val x0 = DimSetVar("X_0")
  val x1 = DimSetVar("X_1")
  val y0 = DimSetVar("Y_0")
  val y1 = DimSetVar("Y_1")

  val (add_dim_a_schema, add_dim_a_ty) = addDimSchema(dim_a)
  val add_dim_a_block = add_dim_a_schema.instantiate

  val (remove_dim_b_schema, remove_dim_b_ty) = removeDimSchema(dim_b)
  val remove_dim_b_block = remove_dim_b_schema.instantiate

  val v0 = add_dim_a_block.freshMapping(add_dim_a_block.schema.inVertices.head)
  val v1 = add_dim_a_block.freshMapping(add_dim_a_block.schema.outVertices.head)

  val v2 = remove_dim_b_block.freshMapping(remove_dim_b_block.schema.inVertices.head)
  val v3 = remove_dim_b_block.freshMapping(remove_dim_b_block.schema.outVertices.head)

  val schema = BlockSchema(
    List(x0, x1),
    List(y0, y1),
    Set(add_dim_a_block, remove_dim_b_block),
    Set(
      x0 -> v0,
      x1 -> v2,
      v1 -> y0,
      v3 -> y1,
    ),
  )

  val typing = collection.mutable.Map(
    add_dim_a_schema -> add_dim_a_ty,
    remove_dim_b_schema -> remove_dim_b_ty,
  )

  (schema, typing)

@main
def funnySchemaExample: BlockTy =
  val (schema, typing) = funnySchema
  inferTypes(schema, typing)

@main
def funnySchemaSequential: BlockTy =
  val (funny_schema, funny_typing) = funnySchema

  val funny_block_1 = funny_schema.instantiate
  val funny_block_2 = funny_schema.instantiate

  val x0 = DimSetVar("X_0")
  val x1 = DimSetVar("X_1")
  val y0 = DimSetVar("Y_0")
  val y1 = DimSetVar("Y_1")


  val v0 = funny_block_1.freshMapping(funny_block_1.schema.inVertices.head)
  val v1 = funny_block_1.freshMapping(funny_block_1.schema.inVertices(1))
  val v2 = funny_block_1.freshMapping(funny_block_1.schema.outVertices.head)
  val v3 = funny_block_1.freshMapping(funny_block_1.schema.outVertices(1))

  val v4 = funny_block_2.freshMapping(funny_block_2.schema.inVertices.head)
  val v5 = funny_block_2.freshMapping(funny_block_2.schema.inVertices(1))
  val v6 = funny_block_2.freshMapping(funny_block_2.schema.outVertices.head)
  val v7 = funny_block_2.freshMapping(funny_block_2.schema.outVertices(1))

  val schema = BlockSchema(
    List(x0, x1),
    List(y0, y1),
    Set(funny_block_1, funny_block_2),
    Set(
      x0 -> v0,
      x1 -> v1,
      v2 -> v5,
      v3 -> v4,
      v6 -> y0,
      v7 -> y1,
    ),
  )

  inferTypes(schema, funny_typing)


@main
def funnySchemaExampleParallel: BlockTy =
  val (funny_schema, funny_typing) = funnySchema
  val (union_schema, union_ty) = unionSchema(4)

  val funny_block_1 = funny_schema.instantiate
  val funny_block_2 = funny_schema.instantiate
  val union_block = union_schema.instantiate

  val x0 = DimSetVar("X_0")
  val x1 = DimSetVar("X_1")
  val y = DimSetVar("Y")

  val v0 = funny_block_1.freshMapping(funny_block_1.schema.inVertices.head)
  val v1 = funny_block_1.freshMapping(funny_block_1.schema.inVertices(1))
  val v2 = funny_block_1.freshMapping(funny_block_1.schema.outVertices.head)
  val v3 = funny_block_1.freshMapping(funny_block_1.schema.outVertices(1))

  val v4 = funny_block_2.freshMapping(funny_block_2.schema.inVertices.head)
  val v5 = funny_block_2.freshMapping(funny_block_2.schema.inVertices(1))
  val v6 = funny_block_2.freshMapping(funny_block_2.schema.outVertices.head)
  val v7 = funny_block_2.freshMapping(funny_block_2.schema.outVertices(1))

  val v8 = union_block.freshMapping(union_block.schema.inVertices.head)
  val v9 = union_block.freshMapping(union_block.schema.inVertices(1))
  val v10 = union_block.freshMapping(union_block.schema.inVertices(2))
  val v11 = union_block.freshMapping(union_block.schema.inVertices(3))
  val v12 = union_block.freshMapping(union_block.schema.outVertices.head)


  val schema = BlockSchema(
    List(x0, x1),
    List(y),
    Set(funny_block_1, funny_block_2, union_block),
    Set(
      x0 -> v0,
      x0 -> v5,
      x1 -> v1,
      x1 -> v4,
      v2 -> v8,
      v3 -> v9,
      v6 -> v10,
      v7 -> v11,
      v12 -> y,
    ),
  )

  funny_typing.put(union_schema, union_ty)
  inferTypes(schema, funny_typing)

@main
def minimalistExample: BlockTy =
  val x0 = DimSetVar("X_0")
  val x1 = DimSetVar("X_1")

  val y = DimSetVar("Y")

  val dim_a = Dim("a")

  val (add_dim_a_schema, add_dim_a_ty) = addDimSchema(dim_a)
  val add_dim_a_block = add_dim_a_schema.instantiate

  val v0 = add_dim_a_block.freshMapping(add_dim_a_block.schema.inVertices.head)
  val v1 = add_dim_a_block.freshMapping(add_dim_a_block.schema.outVertices.head)

  val (union_schema, union_ty) = unionSchema(2)
  val union_block = union_schema.instantiate

  val v2 = union_block.freshMapping(union_block.schema.inVertices.head)
  val v3 = union_block.freshMapping(union_block.schema.inVertices(1))
  val v4 = union_block.freshMapping(union_block.schema.outVertices.head)

  val (remove_dim_a_schema, remove_dim_a_ty) = removeDimSchema(dim_a)
  val remove_dim_a_block = remove_dim_a_schema.instantiate

  val v5 = remove_dim_a_block.freshMapping(remove_dim_a_block.schema.inVertices.head)
  val v6 = remove_dim_a_block.freshMapping(remove_dim_a_block.schema.outVertices.head)

  val schema = BlockSchema(
    List(x0, x1),
    List(y),
    Set(add_dim_a_block, remove_dim_a_block, union_block),
    Set(
      x0 -> v2,
      x1 -> v0,
      v1 -> v3,
      v4 -> v5,
      v6 -> y,
    ),
  )

  val typing = collection.mutable.Map(
    add_dim_a_schema -> add_dim_a_ty,
    remove_dim_a_schema -> remove_dim_a_ty,
    union_schema -> union_ty
  )

  inferTypes(schema, typing)