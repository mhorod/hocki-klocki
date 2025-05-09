package hocki.klocki.typing

import hocki.klocki.typing.Constraint.{InductionNamed, InductionUnnamed, NotIn, In}

object ComposeInductionsNamed extends ConstraintObserver:
  override def observe
  (newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case InductionNamed(dim, from, to) =>
        val lhs = constraints.findInductionsNamedByLhs(dim, to).map(from ~dim~> _.to)
        val rhs = constraints.findInductionsNamedByRhs(dim, from).map(_.from ~dim~> to)
        lhs ++ rhs
      case _ => Set()

object ComposeInductionsUnnamed extends ConstraintObserver:
  override def observe
  (newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case InductionUnnamed(from, to) =>
        val lhs = constraints.findInductionsUnnamedByLhs(to).map(from ~~> _.to)
        val rhs = constraints.findInductionsUnnamedByRhs(from).map(_.from ~~> to)
        lhs ++ rhs
      case _ => Set()


object PropagateNotInsUp extends ConstraintObserver:
  override def observe(newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case NotIn(dim, dimSetVar) =>
        constraints.findInductionsNamedByRhs(dim, dimSetVar).map(induction => NotIn(dim, induction.from))
      case InductionNamed(dim, from, to) =>
        if constraints.contains(NotIn(dim, to)) then
          Set(NotIn(dim, from))
        else Set()
      case _ => Set()

object PropagateInsDown extends ConstraintObserver:
  override def observe(newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case In(dim, dimSetVar) =>
        constraints.findInductionsNamedByLhs(dim, dimSetVar)
          .map(induction => In(dim, induction.to))
      case InductionNamed(dim, from, to) =>
        if constraints.contains(In(dim, from)) then
          Set(In(dim, to))
        else Set()
      case _ => Set()
