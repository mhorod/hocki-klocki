package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}
import hocki.klocki.typing.Constraint.{DependsOnAll, DependsOnDim, In, MinIn, InductionNamed, InductionUnnamed}

import scala.collection.mutable

class Constraints:
  private val mutableConstraints = mutable.Set[Constraint]()

  def constraints: Set[Constraint] = mutableConstraints.toSet

  def addAll(newConstraints: Iterable[Constraint]): Unit = mutableConstraints.addAll(newConstraints)

  def add(constraint: Constraint): Unit = addAll(List(constraint))

  def contains(constraint: Constraint): Boolean = mutableConstraints.contains(constraint)

  def findInductionsNamedByLhs(dim: Dim, dimSetVar: DimSetVar): Set[InductionNamed] =
    constraints.collect {
      case induction: InductionNamed
        if induction.from == dimSetVar && induction.dim == dim => induction
    }

  def findInductionsNamedByRhs(dim: Dim, dimSetVar: DimSetVar): Set[InductionNamed] =
    constraints.collect {
      case induction: InductionNamed
        if induction.to == dimSetVar && induction.dim == dim => induction
    }

  def findInductionsUnnamedByLhs(dimSetVar: DimSetVar): Set[InductionUnnamed] =
    constraints.collect {
      case induction: InductionUnnamed
        if induction.from == dimSetVar => induction
    }


  def findInductionsUnnamedByRhs(dimSetVar: DimSetVar): Set[InductionUnnamed] =
    constraints.collect {
      case induction: InductionUnnamed
        if induction.to == dimSetVar => induction
    }

  def findDependsOnAllByDimSetVar(dimSetVar: DimSetVar): Set[DependsOnAll] =
    constraints.collect {
      case dependsOnAll: DependsOnAll
        if dependsOnAll.filteredDimSetVar.dimSetVar == dimSetVar => dependsOnAll
    }

  def findMinInByDimSetVar(dimSetVar: DimSetVar): Set[MinIn] =
    constraints.collect {
      case minIn: MinIn
        if minIn.filteredDimSetVar.dimSetVar == dimSetVar => minIn
    }

  def findInByDimSetVar(dimSetVar: DimSetVar): Set[In] =
    constraints.collect {
      case in: In
        if in.dimSetVar == dimSetVar => in
    }

  def findInByDim(dim: Dim): Set[In] =
    constraints.collect {
      case in: In
        if in.dim == dim => in
    }

  def findDependencies(dim: Dim): Set[Dim] =
    constraints.collect {
      case dependsOnDim: DependsOnDim
        if dependsOnDim.depender == dim => dependsOnDim.dependency
    }