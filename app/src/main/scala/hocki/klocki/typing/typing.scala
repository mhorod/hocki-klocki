package hocki.klocki.typing

import hocki.klocki.ast.schema.{Primitive, SchemaBinding}
import hocki.klocki.entities.{Dim, DimSetVar}
import hocki.klocki.typing.Constraint.{EquivNamed, In, InUnion, InductionNamed, InductionUnnamed, NotExistential, NotIn}
import hocki.klocki.visualize.{presentConstraints, presentTyping}

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
      if inferred.add(constraint) then
        schemaBindings
          .flatMap(
            b => tagWithSchema(b, observeConstraint(constraint, b, schemata(b), schemaConstraints(b), rules))
          )
          .filterNot(inferred.contains)
          .foreach(worklist.add)

    groupBySchema(inferred.toSet)


class DecomposingPhase(ruleConstructor: Decomposer => Phase)(using ifaceDimSetVars: Set[DimSetVar]) extends Phase:
  override def apply(constraints: SchemaConstraints): SchemaConstraints =
    val decomposer = Decomposer(ifaceDimSetVars, constraints)
    ruleConstructor(decomposer)(constraints)

object SatisfyEquivNamed extends Phase:
  override def apply(constraints: SchemaConstraints): SchemaConstraints =
    given Set[In] = getConstraints[In](constraints)
    val equivsNamed = getConstraints[EquivNamed](constraints)
    constraints.view.mapValues(
      _.flatMap {
        case e@EquivNamed(dim, lhs, rhs) =>
          val deducedLhs = if isSatisfiedUnion(dim, rhs) then Set(dim inUnion lhs) else Set()
          val deducedRhs = if isSatisfiedUnion(dim, lhs) then Set(dim inUnion rhs) else Set()
          val deduced = deducedLhs ++ deducedRhs
          if deduced.isEmpty then Set(e) else deduced
        case other => Set(other)
      }
    ).toMap


object PruneSatisfiedUnionsPhase extends Phase:
  override def apply(constraints: SchemaConstraints): SchemaConstraints =
    given Set[In] = getConstraints[In](constraints)
    constraints.view.mapValues(
      _.filter {
        case InUnion(dim, union) => !isSatisfiedUnion(dim, union)
        case _ => true
      }
    ).toMap

object ReduceUnionsPhase extends Phase:
  override def apply(constraints: SchemaConstraints): SchemaConstraints =
    val notIns = getConstraints[NotIn](constraints)

    constraints.view.mapValues(
      _.map {
        case InUnion(dim, union) =>
          val reduced = dim inUnion union.filterNot(dsv => notIns.contains(dim notIn dsv))
          if reduced.union.isEmpty then
            throw IllegalStateException(s"Typing poszedł w brukselkę: $reduced") // taka unijna rzepa
          reduced
        case other => other
      }
    ).toMap

class GuardExistentialsPhase(schemata: Map[SchemaBinding, Schema]) extends Phase:
  override def apply(constraints: SchemaConstraints): SchemaConstraints =
    constraints.map((schemaBinding, schemaConstraints) =>
      val schema = schemata(schemaBinding)
      val locals = schema.internals.localDims
      schemaBinding -> schemaConstraints.flatMap {
        case in: In => guardLeak(in, schema)
        case inUnion: InUnion => guardLeak(inUnion, schema)
        case notExistential: NotExistential =>
          if schema.allExistentials.contains(notExistential.dim) then
            throw IllegalStateException(s"Typing poszedł w dynię: ${notExistential.dim} cannot be existential")
          Set(notExistential)
        case other => ignoreLeak(other, locals)
      }
    )

private def guardLeak(constraint: Constraint, schema: Schema): Set[Constraint] =
  val leaking = schema.internals.localDims intersect constraint.dims
  val leaks = leaking.nonEmpty && (constraint.dimSetVars subsetOf schema.iface.allDimSetVars)
  if leaks then
    val plural = if leaking.size == 1 then "s" else ""
    throw IllegalStateException(s"Typing poszedł w pora: ${leaking.mkString(", ")} leak$plural through $constraint")
  Set(constraint)

private def ignoreLeak(constraint: Constraint, locals: Set[Dim]): Set[Constraint] =
  if (locals intersect constraint.dims).nonEmpty then
    Set()
  else
    Set(constraint)

def inferTypes(schemata: Map[SchemaBinding, Schema], primitives: Map[Primitive, SchemaBinding]): Map[SchemaBinding, SchemaTy] =

  given Map[SchemaBinding, Schema] = schemata

  val ifaceDimSetVars = schemata.values.map(_.iface).toSet.flatMap(_.allDimSetVars)
  given Set[DimSetVar] = ifaceDimSetVars

  val phases: List[Phase] = List(
    // 0. inductions
    RulesPhase(ComposeInductionsNamed, ComposeInductionsUnnamed),
    // 1. ins down
    RulesPhase(PropagateInsDown),
    // 2. not ins up
    RulesPhase(PropagateNotInsUp),
    // 3. equivs
    SatisfyEquivNamed,
    DecomposingPhase(decomposer => RulesPhase(PropagateEquivsUp(decomposer))),
    // 4. in unions
    PruneSatisfiedUnionsPhase,
    DecomposingPhase(decomposer => RulesPhase(PropagateInUnionsUp(decomposer))),
    ReduceUnionsPhase,
    // error postprocessing
    RulesPhase(RequireDistinct),
    GuardExistentialsPhase(schemata),
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

  val reflConstraints =
    val unnamed = schemata.flatMap((b, s) => s.allDimSetVars.map(dsv => dsv ~~> dsv).map(c => SchemaConstraint(b, c)))
    val named = schemata.flatMap((b, s) => s.allDimSetVars.flatMap(dsv => s.allDims.map(dsv ~ _ ~> dsv)).map(c => SchemaConstraint(b, c)))
    unnamed ++ named


  val initialConstraints = groupBySchema(primitiveConstraints ++ edgeConstraints ++ reflConstraints)


  val inferred = inferInPhases(phases, initialConstraints)

  val tys = inferred.map((b, constraints) =>
    b -> SchemaTy(
      schemata(b).iface,
      filterRelevantConstraints(constraints, schemata(b).iface)
    )
  )

  println(presentTyping(tys))

  tys

def inferInPhases
(
  phases: List[Phase],
  initialConstraints: SchemaConstraints
): SchemaConstraints = phases.foldLeft(initialConstraints)((constraints, phase) => phase(constraints))



def presentSchemaConstraints(constraints: SchemaConstraints): Unit =
  constraints.foreach((b, c) =>
    println(s"Constraints for schema $b:")
    println(presentConstraints(c))
  )

def observeConstraint
(
  constraint: SchemaConstraint,
  schemaBinding: SchemaBinding,
  schema: Schema,
  constraints: Constraints,
  rules: Set[ConstraintObserver]
): Set[Constraint] =
  val renamed = generateRenamedConstraints(schema, constraint)
  if constraint.source == schemaBinding then
    constraints.add(constraint.constraint)
    renamed ++ rules.flatMap(_.observe(constraint.constraint, constraints))
  else
    renamed

def generateRenamedConstraints(schema: Schema, constraint: SchemaConstraint): Set[Constraint] =
  if schema.renamers.contains(constraint.source) then
    schema.renamers(constraint.source).flatMap(_.rename(constraint.constraint))
  else
    Set()