package hocki.klocki.typing

import hocki.klocki.typing.Constraint.{InductionNamed, InductionUnnamed}

object ComposeInductionsNamed extends ConstraintObserver:
  override def observe
  (newConstraint: Constraint)
  (using constraints: Constraints): Set[Constraint] =
    newConstraint match
      case Constraint.InductionNamed(dim, from, to) =>
        val lhs = constraints.findInductionsNamedByLhs(dim, to).map(from ~dim~> _.to)
        val rhs = constraints.findInductionsNamedByRhs(dim, from).map(_.from ~dim~> to)
        lhs ++ rhs
      case _ => Set()

object ComposeInductionsUnnamed extends ConstraintObserver:
  override def observe
  (newConstraint: Constraint)
  (using constraints: Constraints): Set[Constraint] =
    newConstraint match
      case Constraint.InductionUnnamed(from, to) =>
        val lhs = constraints.findInductionsUnnamedByLhs(to).map(from ~~> _.to)
        val rhs = constraints.findInductionsUnnamedByRhs(from).map(_.from ~~> to)
        lhs ++ rhs
      case _ => Set()
