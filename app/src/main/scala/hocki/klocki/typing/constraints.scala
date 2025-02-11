package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

extension (dim: Dim)
  infix def inUnion(union: Set[DimSetVar]): Constraint.InUnion = Constraint.InUnion(dim, union)
  infix def notIn(dimSetVar: DimSetVar): Constraint.NotIn = Constraint.NotIn(dim, dimSetVar)
  infix def isMin(filteredDimSetVar: FilteredDimSetVar): Constraint.IsMin = Constraint.IsMin(dim, filteredDimSetVar)
  infix def isMin(dimSetVar: DimSetVar): Constraint.IsMin = dim isMin (dimSetVar without Set())

extension (dimInCtx: (Dim, DimSetVar))
  infix def dependsOnAll(dimSetVar: FilteredDimSetVar): Constraint.DependsOnAll =
    Constraint.DependsOnAll(dimInCtx._1, dimSetVar, dimInCtx._2)

  infix def dependsOnAll(dimSetVar: DimSetVar): Constraint.DependsOnAll =
    dimInCtx dependsOnAll (dimSetVar without Set())

  infix def dependsOnDim(dim: Dim): Constraint.DependsOnDim =
    Constraint.DependsOnDim(dimInCtx._1, dim, dimInCtx._2)

extension (dimSetVar: DimSetVar)
  infix def inducedBy(inducers: Set[FilteredDimSetVar]): Constraint.InducedBy = Constraint.InducedBy(dimSetVar, inducers)
  infix def without(dims: Set[Dim]): FilteredDimSetVar = FilteredDimSetVar(dimSetVar, dims)
