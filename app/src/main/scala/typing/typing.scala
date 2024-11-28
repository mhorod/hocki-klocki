package hocki.klocki
package typing

import scala.collection.mutable

import semantics.graphs.{BlockSchema, Edge}
import semantics.dims.DimSetVar
import typing.Constraint.InducedBy

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

  println(schema.edges)

  val inductions = inferInductions(constraints)
  inductions.values.foreach(println)

  BlockTy(Set())

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
