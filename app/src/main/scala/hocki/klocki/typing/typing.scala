package hocki.klocki.typing

import hocki.klocki.ast.schema.{Primitive, SchemaBinding}
import hocki.klocki.typing.Constraint.{InductionNamed, InductionUnnamed}

import scala.collection.mutable

private case class SchemaConstraint(source: SchemaBinding, constraint: Constraint)


def inferTypes(schemata: Map[SchemaBinding, Schema], primitives: Map[Primitive, SchemaBinding]): Map[SchemaBinding, SchemaTy] =
  val schemaConstraints: Map[SchemaBinding, Constraints] = schemata.keys.map(_ -> Constraints()).toMap

  val phases = List(
    Set(ComposeInductionsNamed, ComposeInductionsUnnamed),
    Set(PropagateInsDown),
    Set(PropagateNotInsUp),
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

  val initialConstraints = primitiveConstraints ++ edgeConstraints

  val inferred = inferInPhases(schemata, schemaConstraints, phases, initialConstraints)
    .groupMap(_.source)(_.constraint)

  inferred.map((b, constraints) =>
    b -> SchemaTy(
      schemata(b).iface,
      filterRelevantConstraints(constraints, schemata(b).iface)
    )
  )

def inferInPhases
(
  schemata: Map[SchemaBinding, Schema],
  schemaConstraints: Map[SchemaBinding, Constraints],
  phases: List[Set[ConstraintObserver]],
  initialConstraints: Set[SchemaConstraint]
): Set[SchemaConstraint] =
  phases.foldLeft(initialConstraints)(
    (constraints, rules) => inferViaRulesToFixedPoint(schemata, schemaConstraints, rules, constraints)
  )

def inferViaRulesToFixedPoint
(
  schemata: Map[SchemaBinding, Schema],
  schemaConstraints: Map[SchemaBinding, Constraints],
  rules: Set[ConstraintObserver],
  initialConstraints: Set[SchemaConstraint]
): Set[SchemaConstraint] =
  val worklist: mutable.Set[SchemaConstraint] = mutable.Set()
  worklist.addAll(initialConstraints)

  val schemaBindings = schemata.keys

  val inferred: mutable.Set[SchemaConstraint] = mutable.Set()

  while worklist.nonEmpty do
    val constraint = worklist.head
    worklist.remove(constraint)
    inferred.add(constraint)

    schemaBindings.flatMap(
      b => observeConstraint(constraint, b, schemata(b), schemaConstraints(b), rules)
        .map(c => SchemaConstraint(b, c))
    ).foreach(worklist.add)

  inferred.toSet

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




