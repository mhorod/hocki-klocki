package hocki.klocki.typing

import hocki.klocki.analysis.ResolvedNames
import hocki.klocki.ast.Statement.{LocalExistentialDim, SchemaDef}
import hocki.klocki.ast.dim.DimBinding
import hocki.klocki.ast.schema.{Primitive, SchemaBinding, SchemaExpr, SchemaRef}
import hocki.klocki.ast.{Abstra, Statement, Toplevel}
import hocki.klocki.entities.{Dim, DimSetVar, Edge}
import hocki.klocki.typing.Constraint.{DependsOnAll, DependsOnDim, In, InUnion, InducedBy, NotIn, MinIn}
import hocki.klocki.visualize.printConstraints

import scala.collection.immutable
import scala.collection.mutable
import scala.reflect.ClassTag

def inferTypes(toplevel: Toplevel, nr: ResolvedNames): Map[SchemaBinding, SchemaTy] =
  val typing = mutable.Map[SchemaBinding, SchemaTy]()

  given mutable.Map[SchemaBinding, SchemaTy] = typing

  given ResolvedNames = nr

  val schemaDefs = toplevel.statements.collect { case schemaDef: Statement.SchemaDef => schemaDef }
  val bindings =
    schemaDefs.map[(SchemaBinding, SchemaDef)](schemaDef => schemaDef.binding -> schemaDef).toMap

  val globalDims = nr.globalDims.map(binding => binding -> Dim(binding.id.name)).toMap

  given Map[SchemaBinding, SchemaDef] = bindings

  given Map[DimBinding, Dim] = globalDims

  schemaDefs.foreach(schemaDef => inferTypeFor(schemaDef.binding))
  typing.toMap

private def inferTypeFor
(schema: SchemaBinding)
(
  using
  nr: ResolvedNames,
  typing: mutable.Map[SchemaBinding, SchemaTy],
  schemaDefs: Map[SchemaBinding, SchemaDef],
  globalDims: Map[DimBinding, Dim],
)
: Unit =
  if typing.contains(schema) then
    return
  val schemaDef = schemaDefs(schema)
  val impl: Abstra.OnIface = schemaDef.impl match
    case onIface: Abstra.OnIface => onIface
    case _ => throw RuntimeException("Rank 1+ definitions verboten")

  val universals = schemaDef.params.universals.map(binding => binding -> Dim(binding.id.name))
  val existentials = schemaDef.params.existentials.map(binding => binding -> Dim(binding.id.name))
  val localDims = nr.localExistentialDims(schemaDef.binding).map(binding => binding -> Dim(binding.id.name))
  val dimParams = universals.toMap ++ existentials.toMap ++ globalDims ++ localDims

  val ins = impl.iface.suppliers.map(binding => DimSetVar(binding.id.name))
  val outs = impl.iface.consumers.map(binding => DimSetVar(binding.id.name))
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
          val dimMapping = use.expr match
            case ref: SchemaExpr.Leaf =>
              if ref.dimArgs.universals.size != ty.universalDims.size then
                throw IllegalStateException(s"Wrong number of universal dim arguments in use of $blockName in $schema. Expected ${ty.universalDims}, got ${ref.dimArgs.universals}")
              if ref.dimArgs.existentials.size != ty.existentialDims.size then
                throw IllegalStateException(s"Wrong number of existential dim arguments in use of $blockName in $schema. Expected ${ty.existentialDims}, got ${ref.dimArgs.existentials}")
              (ty.universalDims.zip(ref.dimArgs.universals) ++ ty.existentialDims.zip(ref.dimArgs.existentials))
                .map((inType, inRef) => inType -> dimParams(nr.dimNames(inRef)))
                .toMap
            case app: SchemaExpr.App => throw IllegalStateException("Rank 1+ uses verboten")
          val constraints = ty.constraints.map(_.mapDimSetVars(freshMapping).mapDims(dimMapping.withDefault(d => d)))
          (use.iface.allVerticesInOrder.zip(blockIns ++ blockOuts).toMap, constraints)
        case schemaDef: Statement.SchemaDef => throw IllegalStateException("Nested defs verboten")
        case _: LocalExistentialDim => (Map(), Set()) // Do nothing
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
  println(s"Inferring type for schema ${schemaDef.binding.id.name}")
  typing.put(
    schema,
    inferTypeFromConstraints(
      universals.map(_._2),
      existentials.map(_._2),
      localDims.map(_._2),
      ins, outs, constraints, links)
  )

private def getTypeOf
(expr: SchemaExpr)
(using
 nr: ResolvedNames,
 typing: mutable.Map[SchemaBinding, SchemaTy],
 schemaDefs: Map[SchemaBinding, SchemaDef],
 globalDims: Map[DimBinding, Dim],
): SchemaTy =
  expr match
    case leaf: SchemaExpr.Leaf =>
      leaf.schemaRef match
        case named: SchemaRef.Named =>
          val binding = nr.schemaNames(named)
          inferTypeFor(binding)
          typing(binding)
        case builtin: SchemaRef.Builtin => getTypeOfPrimitive(builtin.primitive)
    case app: SchemaExpr.App => throw IllegalStateException("Rank 1+ usages verboten")

private def getTypeOfPrimitive
(primitive: Primitive)
(using
 nr: ResolvedNames,
 globalDims: Map[DimBinding, Dim]
): SchemaTy = primitive match
  case Primitive.Union(arity) =>
    val xs = (0 until arity).map(i => DimSetVar(s"X$i")).toList
    val y = DimSetVar("Y")
    val constraint = y inducedBy xs.toSet.map(_ without Set())
    SchemaTy(List(), List(), xs, List(y), Set(constraint))
  case Primitive.AddNamed(dimRef) =>
    val dim = globalDims(nr.dimNames(dimRef))
    val x = DimSetVar("X")
    val y = DimSetVar("Y")
    val constraints = Set[Constraint](
      dim notIn x,
      dim in y,
      x inducedBy Set(y without Set(dim))
    )
    SchemaTy(List(), List(), List(x), List(y), constraints)
  case Primitive.AddExistential() =>
    val dim = Dim("a")
    val x = DimSetVar("X")
    val y = DimSetVar("Y")
    val constraints = Set[Constraint](
      dim notIn x,
      dim in y,
      x inducedBy Set(y without Set(dim)),
      dim dependsOnAll (x without Set())
    )
    SchemaTy(List(), List(dim), List(x), List(y), constraints)
  case Primitive.Remove() =>
    val dim = Dim("a")
    val x = DimSetVar("X")
    val y = DimSetVar("Y")
    val constraints = Set[Constraint](
      dim in x,
      dim notIn y,
      x inducedBy Set(y without Set(dim)),
      dim minIn (x without Set())
    )
    SchemaTy(List(dim), List(), List(x), List(y), constraints)

private def inferTypeFromConstraints
(
  universalDims: List[Dim],
  existentialDim: List[Dim],
  localDims: Set[Dim],
  inDimSetVars: List[DimSetVar],
  outDimSetVars: List[DimSetVar],
  assumedConstraints: Set[Constraint],
  edges: Set[Edge]
): SchemaTy =
  val immediateConstraints = getImmediateConstraints(inDimSetVars, outDimSetVars, edges)

  val (coalescence, unCoalescence) = coalescing(inDimSetVars, outDimSetVars, edges)
  val constraints = (immediateConstraints ++ assumedConstraints).map(_.mapDimSetVars(coalescence))

  println(edges)
  printConstraints("Assumed constraints (obtained from recursion)", constraints)
  println()

  val directInductions = getConstraints[InducedBy](constraints)
  val inductions = inferInductions(constraints)

  val notIns = inferNotIns(getConstraints[NotIn](constraints), inductions, directInductions)

  val (ins, depsOnAll, depsOnDim) =
    inferInsAndDependencies(
      inductions.values.toSet
        ++ getConstraints[In](constraints)
        ++ getConstraints[DependsOnDim](constraints)
        ++ getConstraints[DependsOnAll](constraints)
    )

  val mins = inferMinima(inductions.values.toSet ++ getConstraints[MinIn](constraints))

  val inUnions =
    pruneUnions(
      inferInUnions(
        getConstraints[InUnion](constraints)
          .filter(inUnion => isNotSatisfied(inUnion, ins)),
        inductions, notIns), ins)


  printConstraints("Ins", ins)
  printConstraints("Not ins", notIns)
  printConstraints("In unions:", inUnions)
  printConstraints("Deps on dim", depsOnDim)
  printConstraints("Deps on all", depsOnAll)
  printConstraints("Mins", mins)

  ins.foreach(
    in => if notIns.contains(NotIn(in.dim, in.dimSetVar)) then
      throw IllegalStateException(s"in / not in clash: $in")
  )

  inUnions.foreach { inUnion =>
    if inUnion.union.isEmpty then
      throw IllegalStateException(s"SUS: $inUnion")
  }

  mins.foreach {
    minIn =>
      depsOnDim.find(depsOnDim => depsOnDim.dependency == minIn.dim && ins.contains(depsOnDim.depender in minIn.filteredDimSetVar.dimSetVar)) match
        case Some(dep) => throw IllegalStateException(s"Typing poszedł w kalarepę, $minIn ale $dep")
        case None => ()
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

  val finalConstraintsWithLocalDims = allConstraints.map(_.mapDimSetVars(unCoalescence)).toSet
  printConstraints("Final constraints (with local dims)", finalConstraintsWithLocalDims)

  val finalConstraints = hideLocalDims(finalConstraintsWithLocalDims, localDims)
  printConstraints("Final constraints", finalConstraints)
  SchemaTy(universalDims, existentialDim, inDimSetVars, outDimSetVars, finalConstraints)

private def inferNotIns(notIns: Set[NotIn], inductions: Map[DimSetVar, InducedBy], directInductions: Set[InducedBy]): Set[NotIn] =
  val inferred = notIns ++ propagateNotInsUp(notIns, inductions)
  inferred ++ propagateNotInsDown(inferred, directInductions)

private def inferInsAndDependencies
(constraints: Set[Constraint]): (Set[In], Set[DependsOnAll], Set[DependsOnDim]) =
  val rules = Set(DependencyIsMember, DependsOnExplicitMember, MemberInduced)
  val inferred = inferViaRulesToFixedPoint(rules, constraints)
  (getConstraints[In](inferred), getConstraints[DependsOnAll](inferred), getConstraints[DependsOnDim](inferred))

private def inferMinima(constraints: Set[Constraint]): Set[MinIn] =
  val inferred = inferViaRulesToFixedPoint(Set(MinInInduction), constraints)
  getConstraints[MinIn](inferred)

private def inferViaRulesToFixedPoint(rules: Iterable[ConstraintObserver], initialConstraints: Iterable[Constraint]): Set[Constraint] =
  val constraints = Constraints()
  val toProcess = mutable.Set.from(initialConstraints)

  given Constraints = constraints

  while toProcess.nonEmpty do
    val constraint = toProcess.head
    toProcess.remove(constraint)
    if !constraints.contains(constraint) then
      println(s"Adding $constraint")
      constraints.add(constraint)
      toProcess.addAll(rules.flatMap(_.observe(constraint)))

  constraints.constraints

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

private def propagateNotInsDown(notIns: Set[NotIn], direct_inductions: Set[InducedBy]): Set[NotIn] = {
  val dims = notIns.map(_.dim)
  val inferredNotIns = notIns.to(mutable.Set)
  var size = -1
  while size != inferredNotIns.size do
    size = inferredNotIns.size
    // for each direct induction Y <= {X1 \ A1, ... }
    // a \notin Y if a \notin all X_i and a \notin all A_i
    for dim <- dims; induction <- direct_inductions do
      if induction.inducers.forall(
        inducer => !inducer.filteredDimensions.contains(dim)
          && inferredNotIns.contains(dim notIn inducer.dimSetVar)) then
        inferredNotIns.add(dim notIn induction.induced)

  inferredNotIns.toSet
}

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

private def hideLocalDims(constraints: Set[Constraint], localDims: Set[Dim]): Set[Constraint] =
  constraints.flatMap {
    case in@Constraint.In(dim, _) =>
      if localDims.contains(dim) then
        throw IllegalStateException(s"typing poszedł w rzepę: $in")
      else
        Some(in)
    case inUnion@Constraint.InUnion(dim, _) =>
      if localDims.contains(dim) then
        throw IllegalStateException(s"typing poszedł w unijną rzepę: $inUnion")
      else
        Some(inUnion)
    case notIn@Constraint.NotIn(dim, _) =>
      if localDims.contains(dim) then
        None // thank god that local dim is not present in the interface :relieved:
      else
        Some(notIn)
    case dependsOnAll@Constraint.DependsOnAll(dim, _) =>
      if localDims.contains(dim) then
        None
      else
        Some(dependsOnAll)
    case dependsOnDim@Constraint.DependsOnDim(depender, dependency) =>
      if localDims.contains(depender) || localDims.contains(dependency) then
        None
      else
        Some(dependsOnDim)
    case Constraint.MinIn(dim, filteredDimSetVar) =>
      if localDims.contains(dim) then
        None
      else
        val constraint = dim minIn
          FilteredDimSetVar(
            filteredDimSetVar.dimSetVar,
            filteredDimSetVar.filteredDimensions.diff(localDims)
          )
        Some(constraint)
    case Constraint.InducedBy(induced, inducers) =>
      val constraint = induced inducedBy
        inducers.map(
          filteredDimSetVar =>
            FilteredDimSetVar(
              filteredDimSetVar.dimSetVar,
              filteredDimSetVar.filteredDimensions.diff(localDims)
            )
        )
      Some(constraint)
  }