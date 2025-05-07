package hocki.klocki.typing

import hocki.klocki.ast.Abstra
import hocki.klocki.entities.DimSetVar
import hocki.klocki.typing.Constraint

import scala.collection.mutable
import scala.reflect.ClassTag

private def inferViaRulesToFixedPoint(rules: Iterable[ConstraintObserver], initialConstraints: Iterable[Constraint]): Set[Constraint] =
  val constraints = Constraints()
  val toProcess = mutable.Set.from(initialConstraints)

  given Constraints = constraints

  while toProcess.nonEmpty do
    val constraint = toProcess.head
    toProcess.remove(constraint)
    if !constraints.contains(constraint) then
      constraints.add(constraint)
      toProcess.addAll(rules.flatMap(_.observe(constraint)))

  constraints.constraints

private def getConstraints[V <: Constraint](constraints: Set[Constraint])(using classTag: ClassTag[V]): Set[V] =
  constraints.collect { case c if classTag.runtimeClass.isInstance(c) => c.asInstanceOf[V] }

private def filterRelevantConstraints
(
  constraints: Set[Constraint],
  ifaceDimSetVars: Set[DimSetVar]
): Set[Constraint] =
  constraints.filter {
    case Constraint.InductionNamed(_, from, to) => ifaceDimSetVars.contains(from) && ifaceDimSetVars.contains(to)
    case Constraint.InductionUnnamed(from, to) => ifaceDimSetVars.contains(from) && ifaceDimSetVars.contains(to)
    case Constraint.In(_, dsv) => ifaceDimSetVars.contains(dsv)
    case Constraint.NotIn(_, dsv) => ifaceDimSetVars.contains(dsv)
    case Constraint.InUnion(_, union) => union subsetOf ifaceDimSetVars
    case _ => false
  }