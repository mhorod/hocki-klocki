package hocki.klocki.ast

import hocki.klocki.utils.Tree

sealed trait AstNode extends Tree

class Toplevel(statements: List[Statement], link: Option[Link]) extends AstNode:
  override def children: List[Tree] = statements ++ link

  override def toString: String = "toplevel"

class VertexDef(val id: VertexId) extends AstNode:
  override def children: List[Tree] = List()

  override def toString: String = s"vertex $id"

class IfaceDef(in: List[VertexDef], out: List[VertexDef]) extends AstNode:
  override def children: List[Tree] = in ++ out

  override def toString: String =
    val inIds = in.map(_.id).mkString(", ")
    val outIds = out.map(_.id).mkString(", ")
    s"[$inIds | $outIds]"

sealed trait Abstra extends AstNode

object Abstra:
  class OnIface(iface: IfaceDef, body: List[Statement], link: Link) extends Abstra:
    override def children: List[Tree] = body :+ link

    override def toString: String = iface.toString

  class OnSchema(schemaId: SchemaId, impl: Abstra) extends Abstra:
    override def children: List[Tree] = List(impl)

    override def toString: String = s"λ $schemaId"

sealed trait Statement extends AstNode

object Statement:
  class SchemaDef(id: SchemaId, impl: Abstra) extends Statement:
    override def children: List[Tree] = List(impl)

    override def toString: String = s"def $id ="

  class BlockUse(expr: SchemaExpr, iface: IfaceDef, name: Option[BlockId]) extends Statement:
    override def children: List[Tree] = List(expr, iface)

    override def toString: String = "use"

sealed trait VertexRef extends AstNode:
  override def children: List[Tree] = List()

object VertexRef:
  class Plain(vertexId: VertexId) extends VertexRef:
    override def toString: String = vertexId.toString

  class Scoped(blockId: BlockId, vertexId: VertexId) extends VertexRef:
    override def toString: String = s"$blockId.$vertexId"

class ConnectionDecl(from: VertexRef, to: VertexRef) extends AstNode:
  override def children: List[Tree] = List(from, to)

  override def toString: String = s"$from >-> $to"

class Link(connections: List[ConnectionDecl]) extends AstNode:
  override def children: List[Tree] = connections

  override def toString: String = "link"

sealed trait SchemaExpr extends AstNode:
  override def children: List[Tree] = List()

object SchemaExpr:
  class Primitive(builtin: BuiltinSchema) extends SchemaExpr:
    override def toString: String = builtin.toString

  class SchemaRef(schemaId: SchemaId) extends SchemaExpr:
    override def toString: String = schemaId.toString

  class App(left: SchemaExpr, right: SchemaExpr) extends SchemaExpr:
    override def children: List[Tree] = List(left, right)

    override def toString: String = "app"
