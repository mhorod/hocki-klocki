package hocki.klocki.typing

import hocki.klocki.typing.Constraint.{DependsOnAll, DependsOnDim, In, InducedBy, MinIn}

//  a --> X \ A    b ∈ X    b not in A
//  ----------------------------------
//              a --> b
//
object DependsOnExplicitMember extends ConstraintObserver:
  override def observe
  (newConstraint: Constraint)
  (using constraints: Constraints): Set[Constraint] =
    newConstraint match
      case In(dim, dimSetVar) =>
        constraints
          .findDependsOnAllByDimSetVar(dimSetVar)
          .filterNot(dependsOnAll => dependsOnAll.filteredDimSetVar.filters(dim))
          .map(dependsOnAll => dependsOnAll.dim dependsOnDim dim)

      case DependsOnAll(dim, filteredDimSetVar) =>
        constraints
          .findInByDimSetVar(filteredDimSetVar.dimSetVar)
          .filterNot(in => filteredDimSetVar.filters(in.dim))
          .map(in => dim dependsOnDim in.dim)
      case _ => Set()

//  a --> Y \ A   Y  ⊇ X \ B
//  ------------------------
//      a --> X \ (A u B)
object DependsOnSubset extends ConstraintObserver:
  override def observe
  (newConstraint: Constraint)
  (using constraints: Constraints): Set[Constraint] =
    newConstraint match
      case InducedBy(induced, inducers) =>
        constraints.findDependsOnAllByDimSetVar(induced)
          .flatMap(depOnAll =>
            inducers.map(
              fdsv => depOnAll.dim dependsOnAll (
                fdsv.dimSetVar without (depOnAll.filteredDimSetVar.filteredDimensions union fdsv.filteredDimensions)
              )
            )
          )
      case DependsOnAll(dim, filteredDimSetVar) =>
        constraints
          .findInducers(filteredDimSetVar.dimSetVar)
          .map(inducedBy =>
            dim dependsOnAll (
              inducedBy.dimSetVar without (filteredDimSetVar.filteredDimensions union inducedBy.filteredDimensions)
            )
          )
      case _ => Set()

//  a --> b    a ∈ X
//  ----------------
//       b ∈ X
object DependencyIsMember extends ConstraintObserver:
  override def observe
  (newConstraint: Constraint)
  (using constraints: Constraints): Set[Constraint] =
    newConstraint match
      case DependsOnDim(depender, dependency) =>
        constraints
          .findInByDim(depender)
          .map(in => dependency in in.dimSetVar)

      case In(dim, dimSetVar) =>
        constraints
          .findDependencies(dim)
          .map(dependency => dependency in dimSetVar)
      case _ => Set()

//   a ∈ X    Y ⊇ X \ A    a not in A
//   --------------------------------
//               a ∈ Y
object MemberInduced extends ConstraintObserver:
  override def observe
  (newConstraint: Constraint)
  (using constraints: Constraints): Set[Constraint] =
    newConstraint match
      case In(dim, dimSetVar) =>
        constraints
          .findInductionsByInducer(dimSetVar)
          .filterNot { case (_, inducer) => inducer.filters(dim) }
          .map { case (induced, _) => dim in induced }
      case InducedBy(dimSetVar, inducers) =>
        inducers.flatMap(inducer =>
          constraints
            .findInByDimSetVar(inducer.dimSetVar)
            .filterNot(in => inducer.filters(in.dim))
            .map(in => in.dim in dimSetVar)
        )
      case _ => Set()

//  a ⟂ Y \ A    Y ⊇ X \ B
//  ------------------------
//      a ⟂ X \ (A ∪ B)
object MinInInduction extends ConstraintObserver:
  override def observe
  (newConstraint: Constraint)
  (using constraints: Constraints): Set[Constraint] =
    newConstraint match
      case MinIn(dim, filteredDimSetVar) =>
        constraints
          .findInducers(filteredDimSetVar.dimSetVar)
          .map(inducer =>
            dim minIn (inducer.dimSetVar without (inducer.filteredDimensions union filteredDimSetVar.filteredDimensions))
          )
      case InducedBy(dimSetVar, inducers) =>
        constraints
          .findMinInByDimSetVar(dimSetVar)
          .flatMap(min =>
            inducers.map(inducer =>
              min.dim minIn (
                inducer.dimSetVar without (inducer.filteredDimensions union min.filteredDimSetVar.filteredDimensions)
              )
            )
          )
      case _ => Set()
