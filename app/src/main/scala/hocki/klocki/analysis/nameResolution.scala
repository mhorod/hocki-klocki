package hocki.klocki.analysis

import hocki.klocki.ast.Statement.{LocalExistentialDim, SchemaDef}
import hocki.klocki.ast.dim.{DimBinding, DimId, DimRef}

import hocki.klocki.ast.schema.{Primitive, SchemaBinding, SchemaExpr, SchemaId, SchemaRef}
import hocki.klocki.ast.vertex.{BlockId, VertexBinding, VertexId, VertexRef}

import scala.collection.mutable
import hocki.klocki.ast.{Abstra, AstNode, ConnectionDecl, GlobalDim, Link, Statement, Toplevel, ToplevelStatement, VertexUse}

import scala.annotation.targetName

enum QuantifiedDim:
  case Universal(dim: DimBinding)
  case Global(dim: DimBinding)
  case Existential(dim: DimBinding)

  def binding: DimBinding = this match
    case Universal(dim) => dim
    case Global(dim) => dim
    case Existential(dim) => dim

class ResolvedNames
(
  val vertexNames: Map[VertexUse, VertexBinding],
  val schemaNames: Map[SchemaRef, SchemaBinding],
  val dimNames: Map[DimRef, DimBinding],
  val globalDims: Set[DimBinding],
  val localExistentialDims: Map[SchemaBinding, Set[DimBinding]]
)

class MutableResolvedNames
(
  val vertexNames: mutable.Map[VertexUse, VertexBinding],
  val schemaNames: mutable.Map[SchemaRef, SchemaBinding],
  val dimNames: mutable.Map[DimRef, DimBinding],
  val globalDims: mutable.Set[DimBinding],
  val localExistentialDims: mutable.Map[SchemaBinding, mutable.Set[DimBinding]]
):
  def toResolvedNames: ResolvedNames =
    ResolvedNames(
      vertexNames.toMap,
      schemaNames.toMap,
      dimNames.toMap,
      globalDims.toSet,
      localExistentialDims.toMap.map((k, v) => k -> v.toSet),
    )


def resolveNames(ast: Toplevel): ResolvedNames =
  val resolved = MutableResolvedNames(mutable.Map(), mutable.Map(), mutable.Map(), mutable.Set(), mutable.Map())
  resolveNames(ast, Context(Map(), Map(), Option.empty, Map(), Map()))(using resolved)
  resolved.toResolvedNames

private def resolveNames(node: AstNode, ctx: Context)(using resolved: MutableResolvedNames): Context =
  node match
    case toplevel: Toplevel =>
      resolveSequential(
        toplevel.statements ++ toplevel.link,
        ctx.withSchemata(toplevelSchemaDefs(toplevel.statements))
          .withGlobalDims(toplevelGlobalDims(toplevel.statements).map(_.binding))
      )
      ctx
    case globalDim: GlobalDim =>
      globalDim.dependsOn.foreach(resolveDim(_, ctx))
      resolved.globalDims.add(globalDim.binding)
      ctx
    case abstra: Abstra =>
      abstra match
        case onIface: Abstra.OnIface =>
          resolveSequential(
            onIface.body :+ onIface.link,
            (ctx + onIface.iface.allVerticesInOrder)
              .withSchemata(schemaDefs(onIface.body))
              .withExistentialDims(localExistentialDims(onIface.body).map(_.binding))
          )
        case onSchema: Abstra.OnSchema => resolveNames(onSchema.impl, ctx + onSchema.binding)
      ctx
    case statement: Statement =>
      statement match
        case schemaDef: Statement.SchemaDef =>
          resolved.localExistentialDims.put(schemaDef.binding, mutable.Set())
          resolveNames(schemaDef.impl,
            ctx
              .withUniversalDims(schemaDef.params.universals)
              .withExistentialDims(schemaDef.params.existentials)
              .withCurrentSchema(schemaDef.binding)
          )
          ctx
        case use: Statement.BlockUse =>
          resolveUsedSchema(use.expr, ctx)
          val all = use.iface.allVerticesInOrder
          use.name match
            case Some(name) => ctx + (name, all)
            case None => ctx + all
        case localExistentialDim: LocalExistentialDim =>
          resolved.localExistentialDims(ctx.currentSchema.get).add(localExistentialDim.binding)
          ctx
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
    case leaf: SchemaExpr.Leaf =>
      leaf.dimArgs.universals.foreach(resolveDim(_, ctx))
      leaf.dimArgs.existentials.foreach(resolveExistentialDim(_, ctx))
      leaf.schemaRef match
        case builtin: SchemaRef.Builtin =>
          builtin.primitive match
            case Primitive.AddNamed(dim) => resolveGlobalDim(dim, ctx)
            case _ => () // Nothing to resolve here
        case named: SchemaRef.Named =>
          ctx(named) match
            case Some(binding) =>
              resolved.schemaNames.put(leaf.schemaRef, binding)
            case None => throw RuntimeException(s"Unresolved reference to schema: ${leaf.schemaRef}")
    case app: SchemaExpr.App =>
      app.children.foreach(resolveUsedSchema(_, ctx))

private def resolveDim(ref: DimRef, ctx: Context)(using resolved: MutableResolvedNames): Unit =
  ctx(ref) match
    case Some(quantifiedDim) => resolved.dimNames.put(ref, quantifiedDim.binding)
    case None => throw RuntimeException(s"Unresolved reference to dim $ref")

private def resolveExistentialDim(ref: DimRef, ctx: Context)(using resolved: MutableResolvedNames): Unit =
  ctx(ref) match
    case Some(QuantifiedDim.Existential(binding)) => resolved.dimNames.put(ref, binding)
    case Some(dim) => throw RuntimeException(s"Expected $ref to refer to existential dim, but found  $dim")
    case None => throw RuntimeException(s"Unresolved reference to dim $ref")


private def resolveGlobalDim(ref: DimRef, ctx: Context)(using resolved: MutableResolvedNames): Unit =
  ctx(ref) match
    case Some(QuantifiedDim.Global(binding)) => resolved.dimNames.put(ref, binding)
    case Some(dim) => throw RuntimeException(s"Expected $ref to refer to global dim, but found  $dim")
    case None => throw RuntimeException(s"Unresolved reference to dim $ref")

private def toplevelSchemaDefs(statements: List[ToplevelStatement]): List[SchemaDef] =
  statements.collect { case s: SchemaDef => s }

private def toplevelGlobalDims(statements: List[ToplevelStatement]): List[GlobalDim] =
  statements.collect { case s: GlobalDim => s }

private def schemaDefs(statements: List[Statement]): List[SchemaDef] =
  statements.collect { case s: SchemaDef => s }

private def localExistentialDims(statements: List[Statement]): List[LocalExistentialDim] =
  statements.collect { case s: LocalExistentialDim => s }

private class Context
(
  plain: Map[VertexId, VertexBinding],
  schemata: Map[SchemaId, SchemaBinding],
  val currentSchema: Option[SchemaBinding],
  scopes: Map[BlockId, Map[VertexId, VertexBinding]],
  dims: Map[DimId, QuantifiedDim]
):

  @targetName("add")
  infix def +(bindings: List[VertexBinding]): Context = Context(plain ++ toNameMap(bindings), schemata, currentSchema, scopes, dims)

  @targetName("add")
  infix def +(blockAndBindings: (BlockId, List[VertexBinding])): Context =
    val (block, bindings) = blockAndBindings
    Context(plain, schemata, currentSchema, scopes + (block -> toNameMap(bindings)), dims)

  @targetName("add")
  infix def +(binding: SchemaBinding): Context =
    Context(plain, schemata + (binding.id -> binding), currentSchema, scopes, dims)

  infix def withSchemata(schemaDefs: List[SchemaDef]): Context =
    Context(plain,
      schemata ++ schemaDefs.map(schemaDef => schemaDef.binding.id -> schemaDef.binding).toMap,
      currentSchema,
      scopes,
      dims)

  def withGlobalDims(bindings: List[DimBinding]): Context =
    Context(
      plain,
      schemata,
      currentSchema,
      scopes,
      dims ++ bindings.map(b => (b.id, QuantifiedDim.Global(b))).toMap
    )

  def withUniversalDims(bindings: List[DimBinding]): Context =
    Context(
      plain,
      schemata,
      currentSchema,
      scopes,
      dims ++ bindings.map(b => (b.id, QuantifiedDim.Universal(b))).toMap
    )

  def withExistentialDims(bindings: List[DimBinding]): Context =
    Context(
      plain,
      schemata,
      currentSchema,
      scopes,
      dims ++ bindings.map(b => (b.id, QuantifiedDim.Existential(b))).toMap
    )

  def withCurrentSchema(schemaBinding: SchemaBinding): Context =
    Context(
      plain,
      schemata,
      Some(schemaBinding),
      scopes,
      dims
    )

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

  def apply(ref: SchemaRef.Named): Option[SchemaBinding] = schemata.get(ref.schemaId)

  def apply(ref: DimRef): Option[QuantifiedDim] = dims.get(ref.dimId)

private def toNameMap(bindings: List[VertexBinding]): Map[VertexId, VertexBinding] =
  bindings.map(b => b.id -> b).toMap