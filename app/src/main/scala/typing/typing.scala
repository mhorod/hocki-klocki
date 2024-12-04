package hocki.klocki
package typing

import scala.collection.mutable
import semantics.graphs.{BlockSchema, Edge}
import semantics.dims.DimSetVar
import typing.Constraint.{InUnion, InducedBy, NotIn}

type Typing = mutable.Map[BlockSchema, BlockTy]

def inferTypes(schema: BlockSchema, typing: Typing): BlockTy =
  if typing.contains(schema) then
    return typing(schema)

  for usedBlock <- schema.blocks do
    if !typing.contains(usedBlock.schema) then
      typing += usedBlock.schema -> inferTypes(schema, typing)

  val coalescence = coalesceConnectedVertices(schema)
  val constraints = schema
    .blocks
    .flatMap(b => typing(b.schema).constraints
      .map(_.map(b.freshMapping))
      .map(_.map(coalescence))
    )

  val unCoalescence = (schema.inVertices.map(v => v -> v) ++ schema.outVertices.map(v => coalescence(v) -> v)).toMap

  println(schema.edges)

  val inductions = inferInductions(constraints)

  val notIns = inferNotIns(constraints, inductions)
  val dimsToNotIns = notIns.groupBy(_.dim)

  val inUnions = inferInUnions(constraints, inductions)
  val reducedInUnions = inUnions.map(
    inUnion =>
      val excluded = dimsToNotIns.getOrElse(inUnion.dim, Set()).map(_.dimSetVar)
      val reduced: InUnion = InUnion(inUnion.dim, inUnion.union diff excluded)
      if reduced.union.isEmpty then
        throw IllegalStateException(s"Absurd constraint $reduced")
      reduced
  )

  println("Inductions:")
  inductions.values.foreach(println)

  println("Not ins:")
  notIns.foreach(println)

  println("In unions:")
  inUnions.foreach(println)

  val allConstraints = inductions.values.map[InducedBy](
    inducedBy => InducedBy(inducedBy.induced, inducedBy.inducers.filter(d => unCoalescence.contains(d.dimSetVar)))
  ).filter(inducedBy => unCoalescence.contains(inducedBy.induced))
  ++ inUnions.map[InUnion](inUnion => InUnion(inUnion.dim, inUnion.union.filter(d => unCoalescence.contains(d))))
    .filter(_.union.nonEmpty)
  ++ notIns.filter(notIn => unCoalescence.contains(notIn.dimSetVar))

  val finalConstraints = allConstraints.map(_.map(unCoalescence))

  println("Final constraints:")
  finalConstraints.foreach(println)

  BlockTy(Set())

def inferInUnions(constraints: Set[Constraint], inductions: Map[DimSetVar, InducedBy]): Set[InUnion] =
  val inUnions = constraints.collect { case constraint: InUnion => constraint }
  val propagatedUp = inUnions ++ propagateInUnionsUp(inUnions, inductions)
  val propagatedDown = propagateInUnionsDown(propagatedUp, inductions.values.toSet)
  propagatedUp ++ propagatedDown

def propagateInUnionsUp(inUnions: Set[InUnion], inductions: Map[DimSetVar, InducedBy]): Set[InUnion] =
 inUnions.map(inUnion =>
      (
        inUnion.dim,
        inUnion.union.flatMap(
          inductions.get(_)
            .map(_.inducers)
            .getOrElse(Set())
            .filter(!_.filteredDimensions.contains(inUnion.dim))
            .map(_.dimSetVar)
        )
      )
    ).filter((dim, union) => union.nonEmpty)
    .map[InUnion]((dim, union) => InUnion(dim, union))

def propagateInUnionsDown(inUnions: Set[InUnion], inductions: Set[InducedBy]): Set[InUnion] =
  inUnions.flatMap {
    inUnion => inductions.filter(
      inducedBy =>
        inUnion.union subsetOf inducedBy.inducers
          .filter(inducer => !inducer.filteredDimensions.contains(inUnion.dim))
          .map(_.dimSetVar)
    ).map(inducedBy => InUnion(inUnion.dim, Set(inducedBy.induced)))
  }

def inferNotIns(constraints: Set[Constraint], inductions: Map[DimSetVar, InducedBy]): Set[NotIn] =
  val notIns = constraints.collect { case constraint: NotIn => constraint }
  val propagatedUp = notIns ++ propagateNotInsUp(notIns, inductions)
  val propagatedDown = propagateNotInsDown(propagatedUp.groupBy(_.dimSetVar), inductions.values.toSet)
  propagatedUp ++ propagatedDown

def propagateNotInsUp(notIns: Set[NotIn], inductions: Map[DimSetVar, InducedBy]): Set[NotIn] =
  notIns.flatMap(
    notIn =>
      inductions.get(notIn.dimSetVar)
        .map(_.inducers)
        .getOrElse(Set())
        .filter(!_.filteredDimensions.contains(notIn.dim))
        .map[NotIn](inducer => NotIn(notIn.dim, inducer.dimSetVar))
  )

def propagateNotInsDown(dimSetVarsToNotIns: Map[DimSetVar, Set[NotIn]], inductions: Set[InducedBy]): Set[NotIn] =
  inductions.flatMap[NotIn] { inducedBy =>
    val filtered = inducedBy.inducers.flatMap(_.filteredDimensions)
    dimSetVarsToNotIns
      .values
      .map(v => v.filter(notIn => !filtered.contains(notIn.dim)).map(_.dim))
      .reduce((x, y) => x intersect y)
      .map(NotIn(_, inducedBy.induced))
  }

def inferInductions(constraints: Set[Constraint]): Map[DimSetVar, InducedBy] =
  val inductions = constraints
    .collect { case constraint: InducedBy => constraint }
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

def joinInductions(lhs: InducedBy, rhs: InducedBy): InducedBy =
  val filtered = lhs.inducers.find(_.dimSetVar == rhs.induced).get.filteredDimensions
  val newInducers = rhs.inducers.map(
    inducer =>
      FilteredDimSetVar(inducer.dimSetVar, inducer.filteredDimensions union filtered)
  )

  val inducers = unifyInducers(lhs.inducers ++ newInducers)
  InducedBy(lhs.induced, inducers)


extension (inducedBy: InducedBy)
  infix def union(other: InducedBy): InducedBy = unionInductions(Set(inducedBy, other))

def unionInductions(inductions: Set[InducedBy]): InducedBy =
  val inducers = unifyInducers(inductions.flatMap(_.inducers))
  InducedBy(inductions.head.induced, inducers)

def unifyInducers(inducers: Set[FilteredDimSetVar]): Set[FilteredDimSetVar] =
  inducers
    .groupBy(_.dimSetVar)
    .map((k, v) => k -> v.map(_.filteredDimensions))
    .map((k, v) => k -> v.reduce(_ intersect _))
    .map((k, v) => FilteredDimSetVar(k, v))
    .toSet

def coalesceConnectedVertices(blockSchema: BlockSchema): Map[DimSetVar, DimSetVar] =
  blockSchema.edges.map(_.swap).toMap ++ blockSchema.edges.map(e => (e(0), e(0))).toMap


def mapEdge(edge: Edge, mapping: Map[DimSetVar, DimSetVar]): Edge = (mapping(edge(0)), mapping(edge(1)))
