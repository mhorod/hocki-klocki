package hocki.klocki
package typing

import semantics.dims.{Dim, DimSetVar}

extension (dim: Dim)
  infix def inUnion(union: Set[DimSetVar]): Constraint.InUnion = Constraint.InUnion(dim, union)
  infix def notIn(dimSetVar: DimSetVar): Constraint.NotIn = Constraint.NotIn(dim, dimSetVar)
  infix def dependsOn(dimSetVar: FilteredDimSetVar): Constraint.DependsOn = Constraint.DependsOn(dim, dimSetVar)

extension (dimSetVar: DimSetVar)
  infix def inducedBy(inducers: Set[FilteredDimSetVar]): Constraint.InducedBy = Constraint.InducedBy(dimSetVar, inducers)
  infix def without(dims: Set[Dim]): FilteredDimSetVar = FilteredDimSetVar(dimSetVar, dims)
