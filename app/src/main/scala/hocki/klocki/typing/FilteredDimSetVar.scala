package hocki.klocki
package typing

import semantics.dims.{Dim, DimSetVar}

// Represents X \ A where X is a dim set variable and A is a known set of dimensions
case class FilteredDimSetVar(val dimSetVar: DimSetVar, val filteredDimensions: Set[Dim]):
  override def toString: String = s"$dimSetVar \\ {${filteredDimensions.mkString(", ")}}"

  def map(mapping: Map[DimSetVar, DimSetVar]): FilteredDimSetVar =
    FilteredDimSetVar(mapping(dimSetVar), filteredDimensions)
