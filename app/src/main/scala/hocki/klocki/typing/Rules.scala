package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}
import hocki.klocki.typing.Constraint.{In, InUnion, InductionNamed, InductionUnnamed, NotIn, EquivNamed, EquivUnnamed}

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
      
class PropagateEquivsUp(decomposer: Decomposer) extends ConstraintObserver:
  override def observe(newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case EquivUnnamed(lhs, rhs) =>
        val decomposedLhs = decomposer.decomposeUnnamed(lhs)
        val decomposedRhs = decomposer.decomposeUnnamed(rhs)
        if decomposedLhs != decomposedRhs then
          Set(decomposedLhs <==> decomposedRhs)
        else
          Set()
      case EquivNamed(dim, lhs, rhs) =>
        val decomposedLhs = decomposer.decomposeNamed(dim, lhs)
        val decomposedRhs = decomposer.decomposeNamed(dim, rhs)
        if decomposedLhs != decomposedRhs then
          Set(decomposedLhs <=| dim |=> decomposedRhs)
        else
          Set()
      case _ => Set()

class PropagateInUnionsUp(decomposer: Decomposer) extends ConstraintObserver:
  override def observe(newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case InUnion(dim, union) =>
        Set(dim inUnion decomposer.decomposeNamed(dim, union))
      case other => Set(other)

object RequireDistinct extends ConstraintObserver:
  override def observe(newConstraint: Constraint, constraints: Constraints): Set[Constraint] =
    newConstraint match
      case notIn@NotIn(dim, dsv) =>
        constraints.findInByDimSetVar(dsv).foreach(in => guardRadish(in, notIn))
      case in@In(dim, dsv) =>
        constraints.findNotInByDimSetVar(dsv).foreach(notIn => guardRadish(in, notIn))
      case _ => ()
    Set()

private def guardRadish(in: In, notIn: NotIn): Unit =
  if in.dim == notIn.dim && in.dimSetVar == notIn.dimSetVar then
    throw new IllegalStateException(s"Typing poszedł w rzodkiew: $in and $notIn")
