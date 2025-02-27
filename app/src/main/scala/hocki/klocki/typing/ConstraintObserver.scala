package hocki.klocki.typing

trait ConstraintObserver:
  def observe(newConstraint: Constraint)(using constraints: Constraints): Set[Constraint]
