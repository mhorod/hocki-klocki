package hocki.klocki.analysis

import hocki.klocki.ast.SchemaExpr.{Primitive, SchemaRef}
import hocki.klocki.ast.Statement.SchemaDef

import scala.collection.mutable
import hocki.klocki.ast.{Abstra, AstNode, BlockId, ConnectionDecl, Link, SchemaBinding, SchemaExpr, SchemaId, Statement, Toplevel, VertexBinding, VertexId, VertexRef, VertexUse}

import scala.annotation.targetName

class ResolvedNames
(
  val vertexNames: Map[VertexUse, VertexBinding],
  val schemaNames: Map[SchemaRef, SchemaBinding]
)

class MutableResolvedNames
(
  val vertexNames: mutable.Map[VertexUse, VertexBinding],
  val schemaNames: mutable.Map[SchemaRef, SchemaBinding]
):
  def toResolvedNames: ResolvedNames = ResolvedNames(vertexNames.toMap, schemaNames.toMap)


def resolveNames(ast: Toplevel): ResolvedNames =
  val resolved = MutableResolvedNames(mutable.Map(), mutable.Map())
  resolveNames(ast, Context(Map(), Map(), Map()))(using resolved)
  resolved.toResolvedNames

private def resolveNames(node: AstNode, ctx: Context)(using resolved: MutableResolvedNames): Context =
  node match
    case toplevel: Toplevel =>
      resolveSequential(toplevel.statements ++ toplevel.link, ctx.withSchemata(schemaDefs(toplevel.statements)))
      ctx
    case abstra: Abstra =>
      abstra match
        case onIface: Abstra.OnIface =>
          resolveSequential(
            onIface.body :+ onIface.link,
            ctx + onIface.iface.allVerticesInOrder
          )
        case onSchema: Abstra.OnSchema => resolveNames(onSchema.impl, ctx + onSchema.binding)
      ctx
    case statement: Statement =>
      statement match
        case schemaDef: Statement.SchemaDef =>
          resolveNames(schemaDef.impl, ctx)
          ctx + schemaDef.binding
        case use: Statement.BlockUse =>
          resolveUsedSchema(use.expr, ctx)
          val all = use.iface.allVerticesInOrder
          use.name match
            case Some(name) => ctx + (name, all)
            case None => ctx + all
    case use: VertexUse =>
      ctx(use.ref) match
        case Some(binding) => resolved.vertexNames.put(use, binding)
        case None => throw RuntimeException(s"Unresolved reference to vertex: ${use.ref}")
      ctx
    case decl: ConnectionDecl =>
      resolveNames(decl.from, ctx)
      resolveNames(decl.to, ctx)
      ctx
    case link: Link =>
      link.connections.foreach(resolveNames(_, ctx))
      ctx

private def resolveSequential(nodes: List[AstNode], ctx: Context)(using mutableResolvedNames: MutableResolvedNames): Context =
  nodes.foldLeft(ctx)((c, n) => resolveNames(n, c))

private def resolveUsedSchema(expr: SchemaExpr, ctx: Context)(using resolved: MutableResolvedNames): Unit =
  expr match
    case primitive: Primitive => ()
    case ref: SchemaRef =>
      ctx(ref) match
        case Some(binding) => resolved.schemaNames.put(ref, binding)
        case None => throw RuntimeException(s"Unresolved reference to schema: $ref")
    case app: SchemaExpr.App =>
      app.children.foreach(resolveUsedSchema(_, ctx))

private def schemaDefs(statements: List[Statement]): List[SchemaDef] =
  statements.collect { case s: SchemaDef => s }

private class Context
(
  plain: Map[VertexId, VertexBinding],
  schemata: Map[SchemaId, SchemaBinding],
  scopes: Map[BlockId, Map[VertexId, VertexBinding]]
):
  @targetName("add")
  infix def +(bindings: List[VertexBinding]): Context = Context(plain ++ toNameMap(bindings), schemata, scopes)

  @targetName("add")
  infix def +(blockAndBindings: (BlockId, List[VertexBinding])): Context =
    val (block, bindings) = blockAndBindings
    Context(plain, schemata, scopes + (block -> toNameMap(bindings)))

  @targetName("add")
  infix def +(binding: SchemaBinding): Context = Context(plain, schemata + (binding.id -> binding), scopes)

  infix def withSchemata(schemaDefs: List[SchemaDef]): Context =
    Context(plain,
      schemata ++ schemaDefs.map(schemaDef => schemaDef.binding.id -> schemaDef.binding).toMap,
      scopes)

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

  def apply(ref: SchemaRef): Option[SchemaBinding] = schemata.get(ref.schemaId)

private def toNameMap(bindings: List[VertexBinding]): Map[VertexId, VertexBinding] =
  bindings.map(b => b.id -> b).toMap