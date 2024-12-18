package hocki.klocki.typing

import hocki.klocki.semantics.dims.{Dim, DimSetVar}

extension (dim: Dim)
  infix def in(dimSetVar: DimSetVar): Constraint.In = Constraint.In(dim, dimSetVar)
  infix def inUnion(union: Set[DimSetVar]): Constraint.InUnion = Constraint.InUnion(dim, union)
  infix def notIn(dimSetVar: DimSetVar): Constraint.NotIn = Constraint.NotIn(dim, dimSetVar)
  infix def dependsOn(dimSetVar: FilteredDimSetVar): Constraint.DependsOn = Constraint.DependsOn(dim, dimSetVar)

extension (dimSetVar: DimSetVar)
  infix def inducedBy(inducers: Set[FilteredDimSetVar]): Constraint.InducedBy = Constraint.InducedBy(dimSetVar, inducers)
  infix def without(dims: Set[Dim]): FilteredDimSetVar = FilteredDimSetVar(dimSetVar, dims)
