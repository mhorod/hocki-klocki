package hocki.klocki.typing

import hocki.klocki.semantics.dims.DimSetVar
import hocki.klocki.semantics.graphs.{BlockSchema, Edge}
import hocki.klocki.typing.Constraint.{In, InUnion, InducedBy, NotIn}

import scala.collection.mutable
import scala.reflect.ClassTag

type Typing = mutable.Map[BlockSchema, BlockTy]

def inferTypes(schema: BlockSchema, typing: Typing): BlockTy =
  println(s"Inferring type of $schema")
  if typing.contains(schema) then
    return typing(schema)

  for usedBlock <- schema.blocks do
    if !typing.contains(usedBlock.schema) then
      println(s"Recursing into $usedBlock")
      typing += usedBlock.schema -> inferTypes(usedBlock.schema, typing)

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

  val ins = inferIns(getConstraints[In](constraints), inductions.values.toSet)
  val notIns = inferNotIns(getConstraints[NotIn](constraints), inductions)
  val inUnions =
    pruneUnions(inferInUnions(getConstraints[InUnion](constraints), inductions, notIns), ins)


  inUnions.foreach { inUnion =>
    if inUnion.union.isEmpty then
      throw IllegalStateException(s"SUS: $inUnion")
  }

  println("Ins:")
  ins.foreach(println)

  println("Not ins:")
  notIns.foreach(println)

  println("In unions:")
  inUnions.foreach(println)

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

  val finalConstraints = allConstraints.map(_.map(unCoalescence))

  println("Final constraints")
  finalConstraints.foreach(println)

  BlockTy(finalConstraints.toSet)

def inferIns(ins: Set[In], inductions: Set[InducedBy]): Set[In] =
  ins ++ propagateInsDown(ins, inductions)

def inferNotIns(notIns: Set[NotIn], inductions: Map[DimSetVar, InducedBy]): Set[NotIn] =
  notIns ++ propagateNotInsUp(notIns, inductions)

def inferInUnions(inUnions: Set[InUnion], inductions: Map[DimSetVar, InducedBy], notIns: Set[NotIn]): Set[InUnion] =
  inUnions ++ propagateInUnionsUp(inUnions, inductions, notIns)

def getConstraints[V <: Constraint](constraints: Set[Constraint])(using classTag: ClassTag[V]): Set[V] =
  constraints.collect { case c if classTag.runtimeClass.isInstance(c) => c.asInstanceOf[V] }

def propagateInsDown(ins: Set[In], inductions: Set[InducedBy]): Set[In] =
  ins ++ ins.flatMap(
    // a \in X
    in => inductions
      // leave inductions Y \supseteq U X \ A where a \notin a
      .filter(_.inducers.exists(inducer => inducer.dimSetVar == in.dimSetVar && !inducer.filteredDimensions.contains(in.dim)))
      .map[In](i => In(in.dim, i.induced))
  )

def propagateNotInsUp(notIns: Set[NotIn], inductions: Map[DimSetVar, InducedBy]): Set[NotIn] =
  notIns.flatMap(
    notIn =>
      inductions.get(notIn.dimSetVar)
        .map(_.inducers)
        .getOrElse(Set())
        .filter(!_.filteredDimensions.contains(notIn.dim))
        .map[NotIn](inducer => NotIn(notIn.dim, inducer.dimSetVar))
  )

def propagateInUnionsUp(inUnions: Set[InUnion], inductions: Map[DimSetVar, InducedBy], notIns: Set[NotIn]): Set[InUnion] =
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

def pruneUnions(inUnions: Set[InUnion], ins: Set[In]): Set[InUnion] =
  inUnions.filter(inUnion => !inUnion.union.exists(elem => ins.contains(inUnion.dim in elem)))

def inferInductions(constraints: Set[Constraint]): Map[DimSetVar, InducedBy] =
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
