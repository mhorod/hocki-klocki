package hocki.klocki.typing

import hocki.klocki.typing.Constraint.{In, InUnion, InductionNamed, InductionUnnamed, NotIn}

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
      case _ => Set()

object PropagateInsDown extends ConstraintObserver:
  override def observe(newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case In(dim, dimSetVar) =>
        constraints.findInductionsNamedByLhs(dim, dimSetVar)
          .map(induction => In(dim, induction.to))
      case _ => Set()

class PropagateInUnionsUp(ifaces: Set[SchemaIface]) extends ConstraintObserver:
  private val ifaceDimSetVars = ifaces.flatMap(_.allDimSetVars)

  override def observe(newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case InUnion(dim, union) =>
        Set(
          dim inUnion union.flatMap(
            dsv => constraints.findInductionsNamedByRhs(dim, dsv)
              .map(_.from)
              .intersect(ifaceDimSetVars)
          )
        )
      case _ => Set()