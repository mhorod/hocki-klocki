package hocki.klocki.semantics.graphs

import hocki.klocki.ast.schema.Primitive
import hocki.klocki.entities.{Dim, DimSetVar}

def namedBuiltinSchema
(
  name: String,
  universalCount: Int,
  existentialCount: Int,
  inCount: Int,
  outCount: Int,
): BlockSchema =
  val inVertices = (0 until inCount).map(i => DimSetVar(s"X$i")).toList
  val outVertices = (0 until outCount).map(i => DimSetVar(s"Y$i")).toList
  val universalDims = (0 until universalCount).map(i => Dim(s"α$i")).toList
  val existentialDims = (0 until existentialCount).map(i => Dim(s"ε$i")).toList
  BlockSchema(BlockSchemaId(), name, universalDims, existentialDims, inVertices, outVertices, Set(), Set())

def fromAst(schema: Primitive): BlockSchema =
  val name = schema.toString
  schema match
    case Primitive.Union(arity) => namedBuiltinSchema(name, 0, 0,  arity, 1)
    case Primitive.AddNamed(_) => namedBuiltinSchema(name, 0, 0, 1, 1)
    case Primitive.AddExistential() => namedBuiltinSchema(name, 0, 1, 1, 1)
    case Primitive.Remove() => namedBuiltinSchema(name, 1, 0, 1, 1)