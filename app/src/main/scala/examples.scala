import hocki.klocki.ast.SchemaId
import hocki.klocki.entities.{Dim, DimSetVar}
import hocki.klocki.names.{NameGenerator, SimpleNameGenerator}
import hocki.klocki.semantics.graphs.{BlockSchema, BlockSchemaId}
import hocki.klocki.typing.{BlockTy, addDimSchema, inferTypes, removeDimSchema, unionSchema}

import scala.collection.mutable

given NameGenerator = SimpleNameGenerator()

def schemaMap(schemata: List[BlockSchema]): Map[BlockSchemaId, BlockSchema] =
  schemata.map(schema => schema.id -> schema).toMap

@main
def chainedDimensionIntroductionsExample: BlockTy =
  val (add_dim_a_schema, add_dim_a_ty) = addDimSchema(Dim("a"))
  val (add_dim_b_schema, add_dim_b_ty) = addDimSchema(Dim("b"))
  val blockA = add_dim_a_schema.instantiate
  val blockB = add_dim_b_schema.instantiate

  val schemata = schemaMap(List(add_dim_a_schema, add_dim_b_schema))

  val v0 = DimSetVar("X")
  val v1 = blockA.freshMapping(schemata(blockA.schemaId).inVertices.head)
  val v2 = blockA.freshMapping(schemata(blockA.schemaId).outVertices.head)
  val v3 = blockB.freshMapping(schemata(blockB.schemaId).inVertices.head)
  val v4 = blockB.freshMapping(schemata(blockB.schemaId).outVertices.head)
  val v5 = DimSetVar("Y")

  val schema = BlockSchema(
    BlockSchemaId(),
    "chained +d",
    List(v0),
    List(v5),
    Set(blockA, blockB),
    Set(v0 -> v1, v2 -> v3, v4 -> v5)
  )
  val typing = collection.mutable.Map(
    add_dim_a_schema.id -> add_dim_a_ty,
    add_dim_b_schema.id -> add_dim_b_ty
  )

  inferTypes(schema, typing)(using schemata)

@main
def parallelDimensionRemovalExample: BlockTy =
  val (add_dim_a_schema, add_dim_a_ty) = removeDimSchema(Dim("a"))
  val (add_dim_b_schema, add_dim_b_ty) = removeDimSchema(Dim("b"))
  val (union_schema, union_ty) = unionSchema(2)

  val schemata = schemaMap(List(add_dim_a_schema, add_dim_b_schema, union_schema))

  val unionBlock1 = union_schema.instantiate
  val unionBlock2 = union_schema.instantiate
  val blockA = add_dim_a_schema.instantiate
  val blockB = add_dim_b_schema.instantiate

  val x0 = DimSetVar("X_0")
  val x1 = DimSetVar("X_1")
  val y = DimSetVar("Y")

  val v0 = unionBlock1.freshMapping(schemata(unionBlock1.schemaId).inVertices.head)
  val v1 = unionBlock1.freshMapping(schemata(unionBlock1.schemaId).inVertices(1))
  val v2 = unionBlock1.freshMapping(schemata(unionBlock1.schemaId).outVertices.head)

  val v3 = blockB.freshMapping(schemata(blockB.schemaId).inVertices.head)
  val v4 = blockB.freshMapping(schemata(blockB.schemaId).outVertices.head)

  val v5 = blockA.freshMapping(schemata(blockA.schemaId).inVertices.head)
  val v6 = blockA.freshMapping(schemata(blockA.schemaId).outVertices.head)

  val v7 = unionBlock2.freshMapping(schemata(unionBlock2.schemaId).inVertices.head)
  val v8 = unionBlock2.freshMapping(schemata(unionBlock2.schemaId).inVertices(1))
  val v9 = unionBlock2.freshMapping(schemata(unionBlock2.schemaId).outVertices.head)


  val schema = BlockSchema(
    BlockSchemaId(),
    "parallel -d",
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
    add_dim_a_schema.id -> add_dim_a_ty,
    add_dim_b_schema.id -> add_dim_b_ty,
    union_schema.id -> union_ty
  )

  inferTypes(schema, typing)(using schemata)

@main
def susExample: BlockTy =
  val dim_a = Dim("a")
  val (add_dim_a_schema, add_dim_a_ty) = addDimSchema(dim_a)
  val (remove_dim_a_schema, remove_dim_a_ty) = removeDimSchema(dim_a)

  val schemata = schemaMap(List(add_dim_a_schema, remove_dim_a_schema))

  val add_dim_a_block = add_dim_a_schema.instantiate
  val remove_dim_a_block = remove_dim_a_schema.instantiate

  val x = DimSetVar("X")
  val y0 = DimSetVar("Y_0")
  val y1 = DimSetVar("Y_1")

  val v0 = add_dim_a_block.freshMapping(schemata(add_dim_a_block.schemaId).inVertices.head)
  val v1 = add_dim_a_block.freshMapping(schemata(add_dim_a_block.schemaId).outVertices.head)

  val v2 = remove_dim_a_block.freshMapping(schemata(remove_dim_a_block.schemaId).inVertices.head)
  val v3 = remove_dim_a_block.freshMapping(schemata(remove_dim_a_block.schemaId).outVertices.head)

  val schema = BlockSchema(
    BlockSchemaId(),
    "sus",
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
    add_dim_a_schema.id -> add_dim_a_ty,
    remove_dim_a_schema.id -> remove_dim_a_ty,
  )

  inferTypes(schema, typing)(using schemata)

def funnySchema: (BlockSchema, mutable.Map[BlockSchemaId, BlockTy], Map[BlockSchemaId, BlockSchema]) =
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

  val schemata = schemaMap(List(add_dim_a_schema, remove_dim_b_schema))

  val v0 = add_dim_a_block.freshMapping(schemata(add_dim_a_block.schemaId).inVertices.head)
  val v1 = add_dim_a_block.freshMapping(schemata(add_dim_a_block.schemaId).outVertices.head)

  val v2 = remove_dim_b_block.freshMapping(schemata(remove_dim_b_block.schemaId).inVertices.head)
  val v3 = remove_dim_b_block.freshMapping(schemata(remove_dim_b_block.schemaId).outVertices.head)

  val schema = BlockSchema(
    BlockSchemaId(),
    "funny",
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
    add_dim_a_schema.id -> add_dim_a_ty,
    remove_dim_b_schema.id -> remove_dim_b_ty,
  )

  (schema, typing, schemata)

@main
def funnySchemaExample: BlockTy =
  val (schema, typing, schemata) = funnySchema
  inferTypes(schema, typing)(using schemata)

@main
def funnySchemaSequential: BlockTy =
  val (funny_schema, funny_typing, innerSchemata) = funnySchema

  val schemata = innerSchemata + (funny_schema.id -> funny_schema)

  val funny_block_1 = funny_schema.instantiate
  val funny_block_2 = funny_schema.instantiate

  val x0 = DimSetVar("X_0")
  val x1 = DimSetVar("X_1")
  val y0 = DimSetVar("Y_0")
  val y1 = DimSetVar("Y_1")


  val v0 = funny_block_1.freshMapping(schemata(funny_block_1.schemaId).inVertices.head)
  val v1 = funny_block_1.freshMapping(schemata(funny_block_1.schemaId).inVertices(1))
  val v2 = funny_block_1.freshMapping(schemata(funny_block_1.schemaId).outVertices.head)
  val v3 = funny_block_1.freshMapping(schemata(funny_block_1.schemaId).outVertices(1))

  val v4 = funny_block_2.freshMapping(schemata(funny_block_2.schemaId).inVertices.head)
  val v5 = funny_block_2.freshMapping(schemata(funny_block_2.schemaId).inVertices(1))
  val v6 = funny_block_2.freshMapping(schemata(funny_block_2.schemaId).outVertices.head)
  val v7 = funny_block_2.freshMapping(schemata(funny_block_2.schemaId).outVertices(1))

  val schema = BlockSchema(
    BlockSchemaId(),
    "funny seq",
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

  inferTypes(schema, funny_typing)(using schemata)


@main
def funnySchemaExampleParallel: BlockTy =
  val (funny_schema, funny_typing, innerSchemata) = funnySchema
  val (union_schema, union_ty) = unionSchema(4)

  val schemata = innerSchemata ++ schemaMap(List(funny_schema, union_schema))

  val funny_block_1 = funny_schema.instantiate
  val funny_block_2 = funny_schema.instantiate
  val union_block = union_schema.instantiate

  val x0 = DimSetVar("X_0")
  val x1 = DimSetVar("X_1")
  val y = DimSetVar("Y")

  val v0 = funny_block_1.freshMapping(schemata(funny_block_1.schemaId).inVertices.head)
  val v1 = funny_block_1.freshMapping(schemata(funny_block_1.schemaId).inVertices(1))
  val v2 = funny_block_1.freshMapping(schemata(funny_block_1.schemaId).outVertices.head)
  val v3 = funny_block_1.freshMapping(schemata(funny_block_1.schemaId).outVertices(1))

  val v4 = funny_block_2.freshMapping(schemata(funny_block_2.schemaId).inVertices.head)
  val v5 = funny_block_2.freshMapping(schemata(funny_block_2.schemaId).inVertices(1))
  val v6 = funny_block_2.freshMapping(schemata(funny_block_2.schemaId).outVertices.head)
  val v7 = funny_block_2.freshMapping(schemata(funny_block_2.schemaId).outVertices(1))

  val v8 = union_block.freshMapping(schemata(union_block.schemaId).inVertices.head)
  val v9 = union_block.freshMapping(schemata(union_block.schemaId).inVertices(1))
  val v10 = union_block.freshMapping(schemata(union_block.schemaId).inVertices(2))
  val v11 = union_block.freshMapping(schemata(union_block.schemaId).inVertices(3))
  val v12 = union_block.freshMapping(schemata(union_block.schemaId).outVertices.head)


  val schema = BlockSchema(
    BlockSchemaId(),
    "funny parallel",
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

  funny_typing.put(union_schema.id, union_ty)
  inferTypes(schema, funny_typing)(using schemata)

@main
def minimalistExample: BlockTy =
  val x0 = DimSetVar("X_0")
  val x1 = DimSetVar("X_1")

  val y = DimSetVar("Y")

  val dim_a = Dim("a")

  val (add_dim_a_schema, add_dim_a_ty) = addDimSchema(dim_a)
  val add_dim_a_block = add_dim_a_schema.instantiate

  val (union_schema, union_ty) = unionSchema(2)
  val union_block = union_schema.instantiate

  val (remove_dim_a_schema, remove_dim_a_ty) = removeDimSchema(dim_a)
  val remove_dim_a_block = remove_dim_a_schema.instantiate

  val schemata = schemaMap(List(add_dim_a_schema, remove_dim_a_schema, union_schema))

  val v0 = add_dim_a_block.freshMapping(schemata(add_dim_a_block.schemaId).inVertices.head)
  val v1 = add_dim_a_block.freshMapping(schemata(add_dim_a_block.schemaId).outVertices.head)


  val v2 = union_block.freshMapping(schemata(union_block.schemaId).inVertices.head)
  val v3 = union_block.freshMapping(schemata(union_block.schemaId).inVertices(1))
  val v4 = union_block.freshMapping(schemata(union_block.schemaId).outVertices.head)


  val v5 = remove_dim_a_block.freshMapping(schemata(remove_dim_a_block.schemaId).inVertices.head)
  val v6 = remove_dim_a_block.freshMapping(schemata(remove_dim_a_block.schemaId).outVertices.head)

  val schema = BlockSchema(
    BlockSchemaId(),
    "minimalist",
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
    add_dim_a_schema.id -> add_dim_a_ty,
    remove_dim_a_schema.id -> remove_dim_a_ty,
    union_schema.id -> union_ty
  )

  inferTypes(schema, typing)(using schemata)