package hocki.klocki.typing

import hocki.klocki.analysis.ResolvedNames
import hocki.klocki.ast.{Abstra, BuiltinSchema, SchemaBinding, SchemaExpr, Statement, Toplevel}
import hocki.klocki.entities.{DimSetVar, Edge}
import hocki.klocki.typing.Constraint.{DependsOnAll, DependsOnDim, InUnion, InducedBy, IsMin, NotIn}

import scala.collection.immutable
import scala.collection.mutable
import scala.reflect.ClassTag

def inferTypes(toplevel: Toplevel, nr: ResolvedNames): Map[SchemaBinding, SchemaTy] =
  val typing = mutable.Map[SchemaBinding, SchemaTy]()

  given mutable.Map[SchemaBinding, SchemaTy] = typing

  given ResolvedNames = nr

  val schemaDefs = toplevel.statements.collect { case schemaDef: Statement.SchemaDef => schemaDef }
  val bindings =
    schemaDefs.map[(SchemaBinding, Abstra.OnIface)](schemaDef =>
      schemaDef.binding -> (
        schemaDef.impl match
          case onIface: Abstra.OnIface => onIface
          case _: Abstra.OnSchema => throw IllegalStateException("Rank 1+ definitions verboten")
        )
    ).toMap

  given Map[SchemaBinding, Abstra.OnIface] = bindings

  schemaDefs.foreach(schemaDef => inferTypeFor(schemaDef.binding))
  typing.toMap

private def inferTypeFor
(schema: SchemaBinding)
(using nr: ResolvedNames, typing: mutable.Map[SchemaBinding, SchemaTy], impls: Map[SchemaBinding, Abstra.OnIface])
: Unit =
  given SchemaBinding = schema
  if typing.contains(schema) then
    return
  val impl = impls(schema)
  val ins = impl.iface.suppliers.map(binding => DimSetVar(binding.id.toString))
  val outs = impl.iface.consumers.map(binding => DimSetVar(binding.id.toString))
  val blocks =
    impl
      .body
      .map {
        case use: Statement.BlockUse =>
          val ty = getTypeOf(use.expr)
          if ty.ins.size != use.iface.consumers.size then
            throw IllegalStateException("Wrong number of consumers")
          if ty.outs.size != use.iface.suppliers.size then
            throw IllegalStateException("Wrong number of suppliers")
          val blockName = use.name.getOrElse("<anon>")
          val blockIns = use.iface.consumers.map(binding => DimSetVar(s"$blockName.${binding.id}"))
          val blockOuts = use.iface.suppliers.map(binding => DimSetVar(s"$blockName.${binding.id}"))
          val freshMapping = (ty.ins.zip(blockIns) ++ ty.outs.zip(blockOuts)).toMap
          val constraints = ty.constraints.map(_.map(freshMapping))
          (use.iface.allVerticesInOrder.zip(blockIns ++ blockOuts).toMap, constraints)
        case schemaDef: Statement.SchemaDef => throw IllegalStateException("Nested defs verboten")
      }
      .toSet
  val bindingsToDimSetVars = impl.iface.allVerticesInOrder.zip(ins ++ outs).toMap ++ blocks.flatMap(_._1)
  val constraints = blocks.flatMap(_._2)
  val links =
    impl
      .link
      .connections
      .map(conn =>
        val from = bindingsToDimSetVars(nr.vertexNames(conn.from))
        val to = bindingsToDimSetVars(nr.vertexNames(conn.to))
        (from, to)
      )
      .toSet
  typing.put(schema, inferTypeFromConstraints(ins, outs, constraints, links))

private def getTypeOf
(expr: SchemaExpr)
(using nr: ResolvedNames, typing: mutable.Map[SchemaBinding, SchemaTy], impls: Map[SchemaBinding, Abstra.OnIface]): SchemaTy =
  expr match
    case primitive: SchemaExpr.Primitive =>
      primitive.builtin match
        case BuiltinSchema.Union(arity) =>
          val xs = (0 until arity).map(i => DimSetVar(s"X$i")).toList
          val y = DimSetVar("Y")
          val constraint = y inducedBy xs.toSet.map(_ without Set())
          SchemaTy(xs, List(y), Set(constraint))
        case BuiltinSchema.Add(dim) =>
          val x = DimSetVar("X")
          val y = DimSetVar("Y")
          val constraints = Set(
            dim notIn x,
            (dim, y) dependsOnAll x,
            y inducedBy Set(x without Set(dim)),
            dim isMin y
          )
          SchemaTy(List(x), List(y), constraints)
        case BuiltinSchema.Remove(dim) =>
          val x = DimSetVar("X")
          val y = DimSetVar("Y")
          val constraints = Set(
            dim inUnion Set(x),
            dim isMin x,
            dim notIn y,
            y inducedBy Set(x without Set(dim))
          )
          SchemaTy(List(x), List(y), constraints)
    case ref: SchemaExpr.SchemaRef =>
      val binding = nr.schemaNames(ref)
      inferTypeFor(binding)
      typing(binding)
    case app: SchemaExpr.App => throw IllegalStateException("Rank 1+ usages verboten")

private def inferTypeFromConstraints
(
  inDimSetVars: List[DimSetVar],
  outDimSetVars: List[DimSetVar],
  assumedConstraints: Set[Constraint],
  edges: Set[Edge]
)
(using environment: SchemaBinding)
: SchemaTy =
  val immediateConstraints = getImmediateConstraints(inDimSetVars, outDimSetVars, edges)

  val (coalescence, unCoalescence) = coalescing(inDimSetVars, outDimSetVars, edges)
  val constraints = (immediateConstraints ++ assumedConstraints).map(_.map(coalescence))

  presentConstraints(s"Constraints from schemata used within ${environment.id}:", constraints)

  val inductions = inferInductions(constraints, edges)
  val dependencies = inferDependencies(getConstraints[DependsOnAll](constraints), inductions)
  val notIns = inferNotIns(getConstraints[NotIn](constraints), inductions)
  val inUnions =
      inferInUnions(
        getConstraints[InUnion](constraints).filterNot(isSatisfied(_, dependencies)),
        inductions,
        notIns
      ).filterNot(isSatisfied(_, dependencies))
  val minima = inferMinima(getConstraints[IsMin](constraints), inductions)
  val directDependencies = getConstraints[DependsOnDim](constraints) ++ inferDirectDependencies(dependencies)

  val allConstraints = inductions.values.toSet ++ dependencies ++ notIns ++ inUnions ++ minima ++ directDependencies
  presentConstraints(s"All constraints for ${environment.id}:", allConstraints)

  assertNoContradictions(allConstraints)

  val relevantInductions =
    inductions
      .values
      .toSet
      .map[InducedBy](
        inducedBy =>
          InducedBy(inducedBy.induced, inducedBy.inducers.filter(d => unCoalescence.contains(d.dimSetVar)))
      )
      .filter(inducedBy => unCoalescence.contains(inducedBy.induced))

  val relevantDependencies = dependencies.filter(dep =>
    unCoalescence.contains(dep.ctx) && unCoalescence.contains(dep.dependency.dimSetVar)
  )
  val relevantNotIns = notIns.filter(notIn => unCoalescence.contains(notIn.dimSetVar))
  val relevantMinima =
    minima
      .filterNot(isMin => isMin.filteredDimSetVar.filteredDimensions.contains(isMin.dim))
      .filter(isMin => unCoalescence.contains(isMin.filteredDimSetVar.dimSetVar))
  val relevantInUnions =
    inUnions
      .map[InUnion](inUnion => InUnion(inUnion.dim, inUnion.union.filter(d => unCoalescence.contains(d))))
      .filter(_.union.nonEmpty)
  val relevantDirectDependencies = directDependencies.filter(dep => unCoalescence.contains(dep.ctx))

  val allRelevantConstraints =
    relevantInductions
      ++ relevantDependencies
      ++ relevantMinima
      ++ relevantNotIns
      ++ relevantInUnions
      ++ relevantDirectDependencies

  val finalConstraints = allRelevantConstraints.map(_.map(unCoalescence))

  presentConstraints(s"Final constraints for ${environment.id}:", finalConstraints)

  SchemaTy(inDimSetVars, outDimSetVars, finalConstraints)

private def assertNoContradictions(constraints: Set[Constraint]): Unit =
  getConstraints[DependsOnAll](constraints).foreach(
    dependency => if constraints.contains(NotIn(dependency.dim, dependency.ctx)) then
      throw IllegalStateException(s"in / not in clash: $dependency (typing poszedł w kalarepę)")
  )

  getConstraints[InUnion](constraints).foreach(inUnion =>
    if inUnion.union.isEmpty then
      throw IllegalStateException(s"membership of empty union: $inUnion (typing poszedł w bataty)")
  )

  val explicitDependencies = getConstraints[DependsOnDim](constraints)
  getConstraints[IsMin](constraints).foreach(isMin =>
    val minimalDependency = explicitDependencies.exists(d =>
      d.dependency == isMin.dim
        && d.ctx == isMin.filteredDimSetVar.dimSetVar
        && !isMin.filteredDimSetVar.filteredDimensions.contains(d.dim)
    )
    if minimalDependency then
      throw IllegalStateException(
        s"dim ${isMin.dim} must be minimal but simultaneously is a dependency (typing poszedł w rzodkiew)"
      )
  )

private def inferDependencies(deps: Set[DependsOnAll], inductions: Map[DimSetVar, InducedBy]): Set[DependsOnAll] =
  val propagatedDown = deps ++ propagateDependenciesDown(deps, inductions)
  propagatedDown ++ propagateDependenciesUp(propagatedDown, inductions)

private def inferMinima(minima: Set[IsMin], inductions: Map[DimSetVar, InducedBy]): Set[IsMin] =
  minima ++ propagateMinimaUp(minima, inductions)

private def inferNotIns(notIns: Set[NotIn], inductions: Map[DimSetVar, InducedBy]): Set[NotIn] =
  notIns ++ propagateNotInsUp(notIns, inductions)

private def inferInUnions(inUnions: Set[InUnion], inductions: Map[DimSetVar, InducedBy], notIns: Set[NotIn]): Set[InUnion] =
  inUnions ++ propagateInUnionsUp(inUnions, inductions, notIns)

private def getConstraints[V <: Constraint](constraints: Set[Constraint])(using classTag: ClassTag[V]): Set[V] =
  constraints.collect { case c if classTag.runtimeClass.isInstance(c) => c.asInstanceOf[V] }

private def isSatisfied(inUnion: InUnion, dependencies: Set[DependsOnAll]): Boolean =
  inUnion.union.exists(dsv => dependencies.exists(dep => dep.dim == inUnion.dim && dsv == dep.ctx))

private def propagateDependenciesDown(deps: Set[DependsOnAll], inductions: Map[DimSetVar, InducedBy]): Set[DependsOnAll] =
  deps.flatMap(dep =>
    inductions
      .values
      .flatMap(inducedBy =>
        inducedBy
          .inducers
          .find(filteredDsv => filteredDsv.dimSetVar == dep.ctx)
          .map(filteredDsv => (inducedBy.induced, filteredDsv.filteredDimensions))
      )
      .filterNot { case (_, filtered) => filtered.contains(dep.dim) }
      .map { case (induced, filtered) =>
        val joinedFiltered = dep.dependency.filteredDimensions union filtered
        (dep.dim, induced) dependsOnAll (dep.dependency.dimSetVar without joinedFiltered)
      }
  )

private def propagateDependenciesUp(deps: Set[DependsOnAll], inductions: Map[DimSetVar, InducedBy]): Set[DependsOnAll] =
  deps
    .flatMap(dep =>
      inductions
        .get(dep.dependency.dimSetVar)
        .map(_.inducers)
        .getOrElse(Set())
        .map(inducer =>
          val joinedFiltered = dep.dependency.filteredDimensions union inducer.filteredDimensions
          (dep.dim, dep.ctx) dependsOnAll (inducer.dimSetVar without joinedFiltered)
        )
    )

private def inferDirectDependencies(deps: Set[DependsOnAll]): Set[DependsOnDim] =
  deps.flatMap(
    a => deps
      .filter(b =>
        a.dim != b.dim
          && a.dependency.dimSetVar == b.ctx
          && !a.dependency.filteredDimensions.contains(b.dim)
      )
      .map(b => (a.dim, a.ctx) dependsOnDim b.dim)
  )

private def propagateMinimaUp(minima: Set[IsMin], inductions: Map[DimSetVar, InducedBy]): Set[IsMin] =
  minima.flatMap(
    isMin => inductions
      .get(isMin.filteredDimSetVar.dimSetVar)
      .map(_.inducers)
      .getOrElse(Set())
      .map[IsMin](inducer => IsMin(isMin.dim, inducer.unionJoin(isMin.filteredDimSetVar)))
  )

private def propagateNotInsUp(notIns: Set[NotIn], inductions: Map[DimSetVar, InducedBy]): Set[NotIn] =
  notIns.flatMap(
    notIn =>
      inductions.get(notIn.dimSetVar)
        .map(_.inducers)
        .getOrElse(Set())
        .filter(!_.filteredDimensions.contains(notIn.dim))
        .map[NotIn](inducer => NotIn(notIn.dim, inducer.dimSetVar))
  )

private def propagateInUnionsUp(inUnions: Set[InUnion], inductions: Map[DimSetVar, InducedBy], notIns: Set[NotIn]): Set[InUnion] =
  inUnions.map(
    inUnion => (
      inUnion.dim,
      inUnion
        .union
        .flatMap(
          elem =>
            inductions
              .get(elem)
              .map(_.inducers)
              .getOrElse(Set(elem without Set()))
              .filter(inducer => !inducer.filteredDimensions.contains(inUnion.dim))
              .map(_.dimSetVar)
        )
        .filter(dimSetVar => !notIns.contains(inUnion.dim notIn dimSetVar))
    )
  )
    .map[InUnion]((dim, union) => InUnion(dim, union))

private def inferInductions(constraints: Set[Constraint], edges: Set[Edge]): Map[DimSetVar, InducedBy] =
  val withEdges = getConstraints[InducedBy](constraints)
    ++ edges.map(e => e._2 inducedBy Set(e._1 without Set()))
  val inductions = withEdges
    .groupBy(_.induced)
    .map((k, v) => k -> unionInductions(v))
    .to(mutable.Map)

  val workList = mutable.Set[(DimSetVar, DimSetVar)]()
  for inducedA <- inductions.keys; inducedB <- inductions.keys do
    if inducedA != inducedB && inductions(inducedA).inducerDimSetVars.contains(inducedB) then
      workList.addOne((inducedA, inducedB))

  while workList.nonEmpty do
    val (inducedA, inducedB) = workList.head
    workList.remove((inducedA, inducedB))

    val joined = joinInductions(inductions(inducedA), inductions(inducedB)) union inductions(inducedA)
    if joined != inductions(inducedA) then
      inductions(inducedA) = joined
      workList.addAll(inductions(inducedA).inducerDimSetVars.filter(inductions.contains).map((inducedA, _)))
      for inducedC <- inductions.keys do
        if inducedC != inducedA && inductions(inducedC).inducerDimSetVars.contains(inducedA) then
          workList.addOne((inducedC, inducedA))

  inductions.toMap

private def joinInductions(lhs: InducedBy, rhs: InducedBy): InducedBy =
  val filtered = lhs.inducers.find(_.dimSetVar == rhs.induced).get.filteredDimensions
  val newInducers = rhs.inducers.map(
    inducer =>
      FilteredDimSetVar(inducer.dimSetVar, inducer.filteredDimensions union filtered)
  )

  val inducers = unifyInducers(lhs.inducers ++ newInducers)
  InducedBy(lhs.induced, inducers)


extension (inducedBy: InducedBy)
  private infix def union(other: InducedBy): InducedBy = unionInductions(Set(inducedBy, other))

private def unionInductions(inductions: Set[InducedBy]): InducedBy =
  val inducers = unifyInducers(inductions.flatMap(_.inducers))
  InducedBy(inductions.head.induced, inducers)

private def unifyInducers(inducers: Set[FilteredDimSetVar]): Set[FilteredDimSetVar] =
  inducers
    .groupBy(_.dimSetVar)
    .map((k, v) => k -> v.map(_.filteredDimensions))
    .map((k, v) => k -> v.reduce(_ intersect _))
    .map((k, v) => FilteredDimSetVar(k, v))
    .toSet

private def coalescing
(
  ins: List[DimSetVar],
  outs: List[DimSetVar],
  edges: Set[Edge],
)
: (Map[DimSetVar, DimSetVar], Map[DimSetVar, DimSetVar]) =
  val coalescence =
    edges.flatMap(e => Set(e._1 -> e._1, e._2 -> e._2)).toMap
    ++ ins.map(v => v -> v).toMap
    ++ outs.map(v => v -> v).toMap
  (coalescence, coalescence)

private def getImmediateConstraints(ins: List[DimSetVar], outs: List[DimSetVar], edges: Set[Edge]): Set[Constraint] =
  val inSet = ins.toSet
  val outSet = outs.toSet
  edges
    .filter(e => inSet.contains(e._1) && outSet.contains(e._2))
    .map(e => InducedBy(e._2, Set(FilteredDimSetVar(e._1, Set()))))