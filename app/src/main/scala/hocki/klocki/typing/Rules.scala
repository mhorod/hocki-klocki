package hocki.klocki.typing

import hocki.klocki.typing.Constraint.{Distinct, In, InUnion, InductionNamed, InductionUnnamed, NotIn}

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
      case in@In(dim, dimSetVar) =>
        constraints.findInductionsNamedByLhs(dim, dimSetVar)
          .map(induction => dim in induction.to)
      case _ => Set()

class PropagateInUnionsUp(ifaces: Set[SchemaIface]) extends ConstraintObserver:
  private val ifaceDimSetVars = ifaces.flatMap(_.allDimSetVars)

  override def observe(newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case InUnion(dim, union) =>
        if union.exists(dsv => constraints.contains(dim in dsv)) then
          Set()
        else
          Set(
            dim inUnion union.flatMap(
              dsv => constraints.findInductionsNamedByRhs(dim, dsv)
                .map(_.from)
                .intersect(ifaceDimSetVars)
            )
          )
      case _ => Set()

object RequireDistinct extends ConstraintObserver:
  override def observe(newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case notIn@NotIn(dim, dsv) =>
        constraints.findInByDimSetVar(dsv).flatMap(in => guardRadish(in, notIn))
      case in@In(dim, dsv) =>
        constraints.findNotInByDimSetVar(dsv).flatMap(notIn => guardRadish(in, notIn))
      case _ => Set()

private def guardRadish(in: In, notIn: NotIn): Set[Distinct] =
  if in.dim == notIn.dim && in.dimSetVar == notIn.dimSetVar then
    throw new IllegalStateException(s"Typing poszedł w rzodkiew: $in and $notIn")
  Set(Distinct(in.dim, notIn.dim), Distinct(notIn.dim, in.dim))

