package hocki.klocki.semantics.graphs

import hocki.klocki.ast.BuiltinSchema
import hocki.klocki.entities.DimSetVar

def builtinSchema(inCount: Int, outCount: Int): BlockSchema =
  val inVertices = (0 until inCount).map(i => DimSetVar(s"X$i")).toList
  val outVertices = (0 until outCount).map(i => DimSetVar(s"Y$i")).toList
  BlockSchema(BlockSchemaId(), inVertices, outVertices, Set(), Set())


def fromAst(schema: BuiltinSchema): BlockSchema =
  schema match
    case BuiltinSchema.Union(arity) => builtinSchema(arity, 1)
    case BuiltinSchema.Add(_) => builtinSchema(1, 1)
    case BuiltinSchema.Remove(_) => builtinSchema(1, 1)