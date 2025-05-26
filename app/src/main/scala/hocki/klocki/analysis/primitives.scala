package hocki.klocki.analysis

import hocki.klocki.Config
import hocki.klocki.ast.schema.{Primitive, SchemaBinding, SchemaId}

def getBindingOfPrimitive(primitive: Primitive): SchemaBinding = primitive match
  case Primitive.Union(arity) => unionPrimitiveBindings(arity)
  case Primitive.Add() => addPrimitiveBinding
  case Primitive.Spawn() => spawnPrimitiveBinding
  case Primitive.Remove() => removePrimitiveBinding
  case Primitive.Join() => joinPrimitiveBinding

private val unionPrimitiveBindings: Vector[SchemaBinding] = Vector.from(
  (0 until Config.MaxUnionWidth).map(n => SchemaBinding(SchemaId(s"U{$n}")))
)
private val addPrimitiveBinding: SchemaBinding = SchemaBinding(SchemaId("+"))
private val spawnPrimitiveBinding: SchemaBinding = SchemaBinding(SchemaId("*"))
private val removePrimitiveBinding: SchemaBinding = SchemaBinding(SchemaId("-"))
private val joinPrimitiveBinding: SchemaBinding = SchemaBinding(SchemaId("><"))
