package hocki.klocki.typing

trait ConstraintObserver:
  def observe(newConstraint: Constraint, constraints: Constraints): Set[Constraint]
