package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

extension (dim: Dim)
  infix def in(dimSetVar: DimSetVar): Constraint.In = Constraint.In(dim, dimSetVar)
  infix def inUnion(union: Set[DimSetVar]): Constraint.InUnion = Constraint.InUnion(dim, union)
  infix def notIn(dimSetVar: DimSetVar): Constraint.NotIn = Constraint.NotIn(dim, dimSetVar)
  infix def dependsOnAll(dimSetVar: FilteredDimSetVar): Constraint.DependsOnAll = Constraint.DependsOnAll(dim, dimSetVar)
  infix def dependsOnDim(dependency: Dim): Constraint.DependsOnDim = Constraint.DependsOnDim(dim, dependency)
  infix def minIn(dimSetVar: FilteredDimSetVar): Constraint.MinIn = Constraint.MinIn(dim, dimSetVar)


extension (dimSetVar: DimSetVar)
  infix def without(dims: Set[Dim]): FilteredDimSetVar = FilteredDimSetVar(dimSetVar, dims)
  infix def ~~>(other: DimSetVar): Constraint.InductionUnnamed = Constraint.InductionUnnamed(dimSetVar, other)
  infix def ~(dim: Dim): PartiallyAppliedInduction = PartiallyAppliedInduction(dimSetVar, dim)

infix case class PartiallyAppliedInduction(dimSetVar: DimSetVar, dim: Dim):
  infix def ~>(other: DimSetVar): Constraint.InductionNamed = Constraint.InductionNamed(dim, dimSetVar, other)

