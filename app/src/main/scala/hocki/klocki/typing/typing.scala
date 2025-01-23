package hocki.klocki.typing

import hocki.klocki.analysis.ResolvedNames
import hocki.klocki.ast.{Abstra, BuiltinSchema, SchemaBinding, SchemaExpr, Statement, Toplevel}
import hocki.klocki.entities.{DimSetVar, Edge}
import hocki.klocki.typing.Constraint.{In, InUnion, InducedBy, NotIn}

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
            dim in y,
            // dim dependsOn (x without Set(dim)),
            y inducedBy Set(x without Set(dim)),
          )
          SchemaTy(List(x), List(y), constraints)
        case BuiltinSchema.Remove(dim) =>
          val x = DimSetVar("X")
          val y = DimSetVar("Y")
          val constraints = Set(
            dim notIn y,
            dim inUnion Set(x),
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
): SchemaTy =
  val immediateConstraints = getImmediateConstraints(inDimSetVars, outDimSetVars, edges)

  val (coalescence, unCoalescence) = coalescing(inDimSetVars, outDimSetVars, edges)
  val constraints = (immediateConstraints ++ assumedConstraints).map(_.map(coalescence))

  println(edges)
  constraints.foreach(println)
  println()

  val inductions = inferInductions(constraints)

  val ins = inferIns(getConstraints[In](constraints), inductions.values.toSet)
  val notIns = inferNotIns(getConstraints[NotIn](constraints), inductions)
  val inUnions =
    pruneUnions(
      inferInUnions(
        getConstraints[InUnion](constraints)
          .filter(inUnion => isNotSatisfied(inUnion, ins)),
        inductions, notIns), ins)


  println("Ins:")
  ins.foreach(println)

  println("Not ins:")
  notIns.foreach(println)

  println("In unions:")
  inUnions.foreach(println)

  inUnions.foreach { inUnion =>
    if inUnion.union.isEmpty then
      throw IllegalStateException(s"SUS: $inUnion")
  }

  val relevantInductions =
    inductions
      .values
      .map[InducedBy](
        inducedBy =>
          InducedBy(inducedBy.induced, inducedBy.inducers.filter(d => unCoalescence.contains(d.dimSetVar)))
      )
      .filter(inducedBy => unCoalescence.contains(inducedBy.induced))

  val relevantIns = ins.filter(in => unCoalescence.contains(in.dimSetVar))
  val relevantNotIns = notIns.filter(notIn => unCoalescence.contains(notIn.dimSetVar))

  val relevantInUnions =
    inUnions
      .map[InUnion](inUnion => InUnion(inUnion.dim, inUnion.union.filter(d => unCoalescence.contains(d))))
      .filter(_.union.nonEmpty)

  val allConstraints = relevantInductions ++ relevantIns ++ relevantNotIns ++ relevantInUnions

  val finalConstraints = allConstraints.map(_.map(unCoalescence)).toSet

  println("Final constraints")
  finalConstraints.foreach(println)

  SchemaTy(inDimSetVars, outDimSetVars, finalConstraints)

private def inferIns(ins: Set[In], inductions: Set[InducedBy]): Set[In] =
  ins ++ propagateInsDown(ins, inductions)

private def inferNotIns(notIns: Set[NotIn], inductions: Map[DimSetVar, InducedBy]): Set[NotIn] =
  notIns ++ propagateNotInsUp(notIns, inductions)

private def inferInUnions(inUnions: Set[InUnion], inductions: Map[DimSetVar, InducedBy], notIns: Set[NotIn]): Set[InUnion] =
  inUnions ++ propagateInUnionsUp(inUnions, inductions, notIns)

private def getConstraints[V <: Constraint](constraints: Set[Constraint])(using classTag: ClassTag[V]): Set[V] =
  constraints.collect { case c if classTag.runtimeClass.isInstance(c) => c.asInstanceOf[V] }

private def propagateInsDown(ins: Set[In], inductions: Set[InducedBy]): Set[In] =
  ins ++ ins.flatMap(
    // a \in X
    in => inductions
      // leave inductions Y \supseteq U X \ A where a \notin a
      .filter(_.inducers.exists(inducer => inducer.dimSetVar == in.dimSetVar && !inducer.filteredDimensions.contains(in.dim)))
      .map[In](i => In(in.dim, i.induced))
  )

private def isNotSatisfied(inUnion: InUnion, ins: Set[In]): Boolean =
  !inUnion.union.exists(dsv => ins.contains(In(inUnion.dim, dsv)))

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

private def pruneUnions(inUnions: Set[InUnion], ins: Set[In]): Set[InUnion] =
  inUnions.filter(inUnion => !inUnion.union.exists(elem => ins.contains(inUnion.dim in elem)))

private def inferInductions(constraints: Set[Constraint]): Map[DimSetVar, InducedBy] =
  val inductions = getConstraints[InducedBy](constraints)
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
  val inSet = ins.toSet
  val outSet = outs.toSet
  val coalescence = outs.zip(outs).toMap ++ (
    edges
      .filterNot(e => inSet.contains(e._1) && outSet.contains(e._2))
      .map(_.swap)
      ++ edges.map(e => (e(0), e(0)))
    ).toMap
  val unCoalescence = (ins.map(v => v -> v) ++ outs.map(v => coalescence(v) -> v)).toMap
  (coalescence, unCoalescence)

private def getImmediateConstraints(ins: List[DimSetVar], outs: List[DimSetVar], edges: Set[Edge]): Set[Constraint] =
  val inSet = ins.toSet
  val outSet = outs.toSet
  edges
    .filter(e => inSet.contains(e._1) && outSet.contains(e._2))
    .map(e => InducedBy(e._2, Set(FilteredDimSetVar(e._1, Set()))))