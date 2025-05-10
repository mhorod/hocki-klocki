package hocki.klocki.typing

import hocki.klocki.ast.schema.{Primitive, SchemaBinding}
import hocki.klocki.typing.Constraint.{In, NotIn, InUnion, InductionNamed, InductionUnnamed}

import scala.collection.mutable

private case class SchemaConstraint(source: SchemaBinding, constraint: Constraint)

type SchemaConstraints = Map[SchemaBinding, Set[Constraint]]
type Phase = SchemaConstraints => SchemaConstraints

class RulesPhase(ruleList: ConstraintObserver*)(using schemata: Map[SchemaBinding, Schema]) extends Phase:
  private val rules = ruleList.toSet

  override def apply(initialConstraints: SchemaConstraints): SchemaConstraints =
    val worklist: mutable.Set[SchemaConstraint] = mutable.Set()

    worklist.addAll(ungroupFromSchema(initialConstraints))

    val schemaBindings = schemata.keys
    val schemaConstraints = schemaBindings.map(b => b -> Constraints(initialConstraints(b))).toMap

    val inferred: mutable.Set[SchemaConstraint] = mutable.Set()

    while worklist.nonEmpty do
      val constraint = worklist.head
      worklist.remove(constraint)
      inferred.add(constraint)

      schemaBindings.flatMap(
          b => tagWithSchema(b, observeConstraint(constraint, b, schemata(b), schemaConstraints(b), rules))
        ).filterNot(inferred.contains)
        .foreach(worklist.add)

    groupBySchema(inferred.toSet)

class PruneSatisfiedUnionsPhase extends Phase:
  override def apply(constraints: SchemaConstraints): SchemaConstraints =
    val ins = getConstraints[In](constraints)
    constraints.view.mapValues(
      _.filter {
        case InUnion(dim, union) => !union.exists(dsv => ins.contains(dim in dsv))
        case other => true
      }
    ).toMap

class ReduceUnionsPhase extends Phase:
  override def apply(constraints: SchemaConstraints): SchemaConstraints =
    val notIns = getConstraints[NotIn](constraints)

    constraints.view.mapValues(
      _.map {
        case InUnion(dim, union) => dim inUnion union.filterNot(dsv => notIns.contains(dim notIn dsv))
        case other => other
      }
    ).toMap

def inferTypes(schemata: Map[SchemaBinding, Schema], primitives: Map[Primitive, SchemaBinding]): Map[SchemaBinding, SchemaTy] =

  given Map[SchemaBinding, Schema] = schemata

  val phases = List(
    RulesPhase(ComposeInductionsNamed, ComposeInductionsUnnamed),
    RulesPhase(PropagateInsDown),
    RulesPhase(PropagateNotInsUp),
    PruneSatisfiedUnionsPhase(),
    RulesPhase(PropagateInUnionsUp(schemata.values.map(_.iface).toSet)),
    ReduceUnionsPhase()
  )

  val primitiveConstraints = primitives.flatMap(
    (primitive, binding) => getTypeOfPrimitive(primitive).constraints.map(c => SchemaConstraint(binding, c))
  ).toSet

  val edgeConstraints = schemata
    .flatMap((b, s) =>
      s.internals.edges
        .flatMap((from, to) => s.allDims.map(dim => InductionNamed(dim, from, to)) + InductionUnnamed(from, to))
        .map(c => SchemaConstraint(b, c))
    )

  val reflConstraints = {
    val unnamed = schemata.flatMap((b, s) => s.allDimSetVars.map(dsv => dsv ~~> dsv).map(c => SchemaConstraint(b, c)))
    val named = schemata.flatMap((b, s) => s.allDimSetVars.flatMap(dsv => s.allDims.map(dsv ~ _ ~> dsv)).map(c => SchemaConstraint(b, c)))
    unnamed ++ named
  }

  val initialConstraints = groupBySchema(primitiveConstraints ++ edgeConstraints ++ reflConstraints)


  val inferred = inferInPhases(phases, initialConstraints)

  inferred.map((b, constraints) =>
    b -> SchemaTy(
      schemata(b).iface,
      filterRelevantConstraints(constraints, schemata(b).iface)
    )
  )

def inferInPhases
(
  phases: List[Phase],
  initialConstraints: SchemaConstraints
): SchemaConstraints =
  phases.foldLeft(initialConstraints)((constraints, phase) => phase(constraints))

def observeConstraint
(
  constraint: SchemaConstraint,
  schemaBinding: SchemaBinding,
  schema: Schema,
  constraints: Constraints,
  rules: Set[ConstraintObserver]
): Set[Constraint] =
  if constraint.source == schemaBinding then
    constraints.add(constraint.constraint)
    rules.flatMap(_.observe(constraint.constraint, constraints))
  else if schema.renamers.contains(constraint.source) then
    schema.renamers(constraint.source).flatMap(_.rename(constraint.constraint))
  else
    Set()
