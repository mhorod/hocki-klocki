package hocki.klocki.analysis

import scala.collection.mutable
import hocki.klocki.ast.{Abstra, AstNode, BlockId, ConnectionDecl, Link, Statement, Toplevel, VertexBinding, VertexId, VertexRef, VertexUse}

import scala.annotation.targetName

type ResolvedNames = Map[VertexUse, VertexBinding]

def resolveNames(ast: Toplevel): ResolvedNames =
  val resolved = mutable.Map[VertexUse, VertexBinding]()
  resolveNames(ast, Context(Map(), Map()))(using resolved)
  resolved.toMap

private def resolveNames(node: AstNode, ctx: Context)(using resolved: mutable.Map[VertexUse, VertexBinding]): Context =
  node match
    case toplevel: Toplevel =>
      resolveSequential(toplevel.statements ++ toplevel.link, ctx)
      ctx
    case abstra: Abstra =>
      abstra match
        case onIface: Abstra.OnIface =>
          resolveSequential(onIface.body :+ onIface.link, ctx + onIface.iface.allVerticesInOrder)
        case onSchema: Abstra.OnSchema => resolveNames(onSchema.impl, ctx)
      ctx
    case statement: Statement =>
      statement match
        case schemaDef: Statement.SchemaDef =>
          resolveNames(schemaDef.impl, ctx)
          ctx
        case use: Statement.BlockUse =>
          val all = use.iface.allVerticesInOrder
          use.name match
            case Some(name) => ctx + (name, all)
            case None => ctx + all
    case use: VertexUse =>
      ctx(use.ref) match
        case Some(binding) => resolved.put(use, binding)
        case None => throw RuntimeException("SUS")
      ctx
    case decl: ConnectionDecl =>
      resolveNames(decl.from, ctx)
      resolveNames(decl.to, ctx)
      ctx
    case link: Link =>
      link.connections.foreach(resolveNames(_, ctx))
      ctx

private def resolveSequential(nodes: List[AstNode], ctx: Context)(using mutable.Map[VertexUse, VertexBinding]) =
  nodes.foldLeft(ctx)((c, n) => resolveNames(n, c))

private class Context(plain: Map[VertexId, VertexBinding], scopes: Map[BlockId, Map[VertexId, VertexBinding]]):
  @targetName("add")
  infix def +(bindings: List[VertexBinding]): Context = Context(plain ++ toNameMap(bindings), scopes)

  @targetName("add")
  infix def +(blockAndBindings: (BlockId, List[VertexBinding])): Context =
    val (block, bindings) = blockAndBindings
    Context(plain, scopes + (block -> toNameMap(bindings)))

  def apply(ref: VertexRef): Option[VertexBinding] =
    ref match
      case VertexRef.Plain(vertexId) =>
        plain.get(vertexId) match
          case None =>
            val candidates = scopes.values.filter(_.contains(vertexId))
            if candidates.size == 1 then Some(candidates.head(vertexId)) else None
          case some => some
      case VertexRef.Scoped(blockId, vertexId) =>
        for
          scope <- scopes.get(blockId)
          resolved <- scope.get(vertexId)
        yield resolved

private def toNameMap(bindings: List[VertexBinding]): Map[VertexId, VertexBinding] =
  bindings.map(b => b.id -> b).toMap