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

  println(inferInductions(constraints))

  BlockTy(Set())

def inferInductions(constraints: Set[Constraint]): Map[DimSetVar, InducedBy] =
  val inductions = constraints
    .collect { case constraint: InducedBy => constraint }
    .groupBy(_.induced)
    .map((k, v) => k -> unionInductions(v))
    .to(mutable.Map)

  var inProgress = true
  while inProgress do
    inProgress = false
    for inducedA <- inductions.keys; inducedB <- inductions.keys do
      val inductionA = inductions(inducedA)
      val inductionB = inductions(inducedB)
      if inductionA.inducerDimSetVars.contains(inducedB) then
        ???

  inductions.toMap

def joinInductions(by: InducedBy, by1: InducedBy): InducedBy = ???

def unionInductions(inductions: Set[InducedBy]): InducedBy =
  val inducers = inductions
    .flatMap(_.inducers)
    .groupBy(_.dimSetVar)
    .map((k, v) => k -> v.map(_.filteredDimensions))
    .map((k, v) => k -> v.reduce(_ intersect _))
    .map((k, v) => FilteredDimSetVar(k, v))
    .toSet
  InducedBy(inductions.head.induced, inducers)


def coalesceConnectedVertices(blockSchema: BlockSchema): Map[DimSetVar, DimSetVar] =
  blockSchema.edges.map(_.swap).toMap ++ blockSchema.edges.map(e => (e(0), e(0))).toMap


def mapEdge(edge: Edge, mapping: Map[DimSetVar, DimSetVar]): Edge = (mapping(edge(0)), mapping(edge(1)))
