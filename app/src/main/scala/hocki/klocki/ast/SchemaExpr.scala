package hocki.klocki.ast

import hocki.klocki.utils.Tree

sealed trait SchemaExpr extends Tree:
  override def children: List[SchemaExpr] = List()

object SchemaExpr:
  class Primitive(builtin: BuiltinSchema) extends SchemaExpr:
    override def toString: String = builtin.toString

  class SchemaRef(val schemaId: SchemaId) extends SchemaExpr:
    override def toString: String = schemaId.toString

  class App(left: SchemaExpr, right: SchemaExpr) extends SchemaExpr:
    override def children: List[SchemaExpr] = List(left, right)

    override def toString: String = "app"
