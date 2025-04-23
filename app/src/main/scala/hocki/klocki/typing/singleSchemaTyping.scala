package hocki.klocki.typing

import hocki.klocki.ast.schema.SchemaBinding
import hocki.klocki.entities.{Dim, DimSetVar, Edge}
import hocki.klocki.typing.Constraint.{In, InUnion, InductionNamed, InductionUnnamed, NotIn}
import hocki.klocki.visualize.printConstraints

def inferTypeFromConstraints
(
  schemaBinding: SchemaBinding,
  universalDims: List[Dim],
  existentialDim: List[Dim],
  localDims: Set[Dim],
  usedDims: Set[Dim],
  allDimSetVars: Set[DimSetVar],
  inDimSetVars: List[DimSetVar],
  outDimSetVars: List[DimSetVar],
  assumedConstraints: Set[Constraint],
  edges: Set[Edge]
): SchemaTy =
  val ifaceDimSetVars = (inDimSetVars ++ outDimSetVars).toSet

  val withInductions = inferInductions(assumedConstraints, edges, usedDims)

  val namedInductions = getConstraints[InductionNamed](withInductions)
  val unnamedInductions = getConstraints[InductionUnnamed](withInductions)

  val ins = propagateInsDown(getConstraints[In](assumedConstraints), namedInductions, allDimSetVars)
  val notIns = propagateNotInsUp(getConstraints[NotIn](assumedConstraints), namedInductions, allDimSetVars)
  val inUnions = propagateInUnionsUp(
    removeSatisfiedUnions(
      getConstraints[InUnion](assumedConstraints),
      ins,
    ),
    namedInductions,
    notIns,
    ifaceDimSetVars,
  )

  val allConstraints = namedInductions ++ unnamedInductions ++ ins ++ notIns ++ inUnions

  val relevantConstraints = filterRelevantConstraints(
    allConstraints,
    ifaceDimSetVars,
  )

  checkForContradictions(allConstraints)

  println(s"\nTyping ${schemaBinding.id}")
  printConstraints("Assumed constraints (obtained from recursion)", assumedConstraints)
  printConstraints("Named inductions", namedInductions)
  printConstraints("Unnamed inductions", unnamedInductions)
  printConstraints("Ins", ins)
  printConstraints("Not ins", notIns)
  printConstraints("In unions", inUnions)
  printConstraints(s"${schemaBinding.id} : [${inDimSetVars.mkString(", ")} | ${outDimSetVars.mkString(", ")}]", relevantConstraints)

  SchemaTy(universalDims, existentialDim, usedDims, inDimSetVars, outDimSetVars, relevantConstraints)

private def inferInductions(constraints: Set[Constraint], edges: Set[Edge], usedDims: Set[Dim]): Set[Constraint] =
  val rules = List(ComposeInductionsNamed, ComposeInductionsUnnamed)
  inferViaRulesToFixedPoint(
    rules,
    constraints ++ edges.flatMap { case (u, v) => usedDims.map(u ~_~> v) + (u ~~> v) },
  )

private def propagateInsDown(ins: Set[In], inductions: Set[InductionNamed], iface: Set[DimSetVar]): Set[In] =
  ins ++ ins.flatMap(
    c => iface
      .filter(dsv => inductions.contains(c.dimSetVar ~c.dim~> dsv))
      .map(dsv => c.dim in dsv)
  )

private def propagateNotInsUp(notIns: Set[Constraint.NotIn], inductions: Set[InductionNamed], dsvs: Set[DimSetVar]): Set[NotIn] =
  notIns ++ notIns.flatMap(
    c => dsvs
      .filter(dsv => inductions.contains(dsv ~c.dim~> c.dimSetVar))
      .map(dsv => c.dim notIn dsv)
  )

private def propagateInUnionsUp(inUnions: Set[Constraint.InUnion], inductions: Set[InductionNamed], notIns: Set[Constraint.NotIn], dsvs: Set[DimSetVar]): Set[InUnion] =
  inUnions.map(c =>
    val union =
      c
        .union
        .flatMap(unionDsv => dsvs.filter(dsv => inductions.contains(dsv ~c.dim~> unionDsv)))
        .filterNot(dsv => notIns.contains(c.dim notIn dsv))
    c.dim inUnion union
  )

private def removeSatisfiedUnions(inUnions: Set[InUnion], ins: Set[In]): Set[InUnion] =
  inUnions.filterNot(isUnionSatisfied(_, ins))

private def isUnionSatisfied(inUnion: InUnion, ins: Set[In]): Boolean =
  inUnion.union.exists(dsv => ins.contains(inUnion.dim in dsv))

private def checkForContradictions(constraints: Set[Constraint]): Unit =
  constraints.foreach {
    case in@In(dim, dimSetVar) =>
      val clashing = dim notIn dimSetVar
      if constraints.contains(clashing) then
        throw IllegalStateException(s"Typing poszedł w rzodkiew: $in vs $clashing")
    case inUnion@Constraint.InUnion(dim, union) =>
      if union.isEmpty then
        throw IllegalStateException(s"Typing poszedł w rzepę: $inUnion")
    case _ => ()
  }
