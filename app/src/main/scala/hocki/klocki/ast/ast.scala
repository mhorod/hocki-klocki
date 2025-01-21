package hocki.klocki.ast

import hocki.klocki.utils.{Tree, toParenthesesString}

sealed trait AstNode extends Tree:
  override def children: List[AstNode]

class Toplevel(val statements: List[Statement], val link: Option[Link]) extends AstNode:
  override def children: List[AstNode] = statements ++ link

  override def toString: String = "toplevel"

sealed trait Abstra extends AstNode

object Abstra:
  class OnIface(val iface: IfaceBinding.Internal, val body: List[Statement], val link: Link) extends Abstra:
    override def children: List[AstNode] = body :+ link

    override def toString: String = iface.toString

  class OnSchema(val binding: SchemaBinding, val impl: Abstra) extends Abstra:
    override def children: List[Abstra] = List(impl)

    override def toString: String = binding.toString

sealed trait Statement extends AstNode

object Statement:
  class SchemaDef(val binding: SchemaBinding, val impl: Abstra) extends Statement:
    override def children: List[Abstra] = List(impl)

    override def toString: String = s"def ${binding} ="

  class BlockUse(val expr: SchemaExpr, val iface: IfaceBinding.External, val name: Option[BlockId]) extends Statement:
    override def children: List[Nothing] = List()

    override def toString: String = s"use ${expr.toParenthesesString} $iface"

sealed abstract class VertexUse(val ref: VertexRef) extends AstNode:
  override def children: List[Nothing] = List()

  override def toString: String = ref.toString

object VertexUse:
  class Supplier(ref: VertexRef) extends VertexUse(ref)

  class Consumer(ref: VertexRef) extends VertexUse(ref)

class ConnectionDecl(val from: VertexUse, val to: VertexUse) extends AstNode:
  override def children: List[VertexUse] = List(from, to)

  override def toString: String = ">->"

class Link(val connections: List[ConnectionDecl]) extends AstNode:
  override def children: List[ConnectionDecl] = connections

  override def toString: String = "link"

