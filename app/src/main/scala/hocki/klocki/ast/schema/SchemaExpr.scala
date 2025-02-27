package hocki.klocki.ast.schema

import hocki.klocki.ast.dim.DimArgs
import hocki.klocki.ast.schema.{Primitive, SchemaExpr, SchemaId, SchemaRef}
import hocki.klocki.utils.Tree

sealed trait SchemaExpr extends Tree:
  override def children: List[SchemaExpr] = List()

object SchemaExpr:
  class Leaf(val schemaRef: SchemaRef, val dimArgs: DimArgs) extends SchemaExpr

  class App(val left: SchemaExpr, val right: SchemaExpr) extends SchemaExpr:
    override def children: List[SchemaExpr] = List(left, right)

    override def toString: String = "app"

sealed trait SchemaRef

object SchemaRef:
  class Builtin(val primitive: Primitive) extends SchemaRef:
    override def toString: String = primitive.toString

  class Named(val schemaId: SchemaId) extends SchemaRef:
    override def toString: String = schemaId.toString