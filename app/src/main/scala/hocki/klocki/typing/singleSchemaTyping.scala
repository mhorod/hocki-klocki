package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar, Edge}
import hocki.klocki.typing.Constraint.{In, InUnion, InductionNamed, NotIn}
import hocki.klocki.visualize.printConstraints

def inferTypeFromConstraints
(
  universalDims: List[Dim],
  existentialDim: List[Dim],
  localDims: Set[Dim],
  inDimSetVars: List[DimSetVar],
  outDimSetVars: List[DimSetVar],
  assumedConstraints: Set[Constraint],
  edges: Set[Edge]
): SchemaTy =
  printConstraints("Assumed constraints (obtained from recursion)", assumedConstraints)

  val ifaceDimSetVars = (inDimSetVars ++ outDimSetVars).toSet

  val inductions = inferInductions()
  val namedInductions = getConstraints[InductionNamed](inductions)

  val ins = propagateInsDown(getConstraints[In](assumedConstraints), namedInductions, ifaceDimSetVars)
  val notIns = propagateNotInsUp(getConstraints[NotIn](assumedConstraints), namedInductions, ifaceDimSetVars)
  val inUnions = propagateInUnionsUp(
    removeSatisfiedUnions(
      getConstraints[InUnion](assumedConstraints),
      ins,
    ),
    namedInductions,
    notIns,
    ifaceDimSetVars,
  )

  val relevantConstraints = filterRelevantConstraints(
    inductions ++ ins ++ notIns ++ inUnions,
    ifaceDimSetVars,
  )

  SchemaTy(universalDims, existentialDim, inDimSetVars, outDimSetVars, relevantConstraints)

private def removeSatisfiedUnions(inUnions: Set[InUnion], ins: Set[In]): Set[InUnion] =
  inUnions.filterNot(isUnionSatisfied(_, ins))

private def isUnionSatisfied(inUnion: InUnion, ins: Set[In]): Boolean =
  inUnion.union.exists(dsv => ins.contains(inUnion.dim in dsv))

private def propagateInsDown(ins: Set[In], inductions: Set[InductionNamed], iface: Set[DimSetVar]): Set[In] =
  ins.flatMap(
    c => iface
      .filter(dsv => inductions.contains(c.dimSetVar ~c.dim~> dsv))
      .map(dsv => c.dim in dsv)
  )

private def propagateNotInsUp(notIns: Set[Constraint.NotIn], inductions: Set[InductionNamed], iface: Set[DimSetVar]): Set[NotIn] =
  notIns.flatMap(
    c => iface
      .filter(dsv => inductions.contains(dsv ~c.dim~> dsv))
      .map(dsv => c.dim notIn dsv)
  )

private def propagateInUnionsUp(inUnions: Set[Constraint.InUnion], inductions: Set[InductionNamed], notIns: Set[Constraint.NotIn], iface: Set[DimSetVar]): Set[InUnion] =
  inUnions.map(c =>
    val union =
      c
        .union
        .flatMap(unionDsv => iface.filter(dsv => inductions.contains(dsv ~c.dim~> unionDsv)))
        .filterNot(dsv => notIns.contains(c.dim notIn dsv))
    c.dim inUnion union
  )