package hocki.klocki.visualize.graph

import hocki.klocki.ast.schema.Primitive
import hocki.klocki.entities.{Dim, DimSetVar}

def namedBuiltinSchema
(
  name: String,
  universalCount: Int,
  existentialCount: Int,
  inCount: Int,
  outCount: Int,
)(using idGenerator: IdGenerator): Schema =
  val inVertices = (0 until inCount).map(i => idGenerator.dimSetVarId(s"X$i")).toList
  val outVertices = (0 until outCount).map(i => idGenerator.dimSetVarId(s"Y$i")).toList
  val universalDims = (0 until universalCount).map(i => idGenerator.dimId(s"α$i")).toList
  val existentialDims = (0 until existentialCount).map(i => idGenerator.dimId(s"ε$i")).toList
  val interface = SchemaInterface(universalDims, existentialDims, inVertices, outVertices)
  Schema(idGenerator.schemaId, name, interface, Set(), Set())

def fromAst(schema: Primitive)(using idGenerator: IdGenerator): Schema =
  val name = schema.toString
  schema match
    case Primitive.Union(arity) => namedBuiltinSchema(name, 0, 0, arity, 1)
    case Primitive.Add() => namedBuiltinSchema(name, 0, 0, 1, 1)
    case Primitive.Spawn() => namedBuiltinSchema(name, 0, 1, 1, 1)
    case Primitive.Remove() => namedBuiltinSchema(name, 1, 0, 1, 1)