package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

// Represents X \ A where X is a dim set variable and A is a known set of dimensions
case class FilteredDimSetVar(dimSetVar: DimSetVar, filteredDimensions: Set[Dim]):
  override def toString: String = s"$dimSetVar \\ {${filteredDimensions.mkString(", ")}}"

  def filters(dim: Dim): Boolean = filteredDimensions.contains(dim)
  
  def mapDimSetVars(mapping: Map[DimSetVar, DimSetVar]): FilteredDimSetVar =
    FilteredDimSetVar(mapping(dimSetVar), filteredDimensions)

  def mapDims(mapping: Map[Dim, Dim]): FilteredDimSetVar =
    FilteredDimSetVar(dimSetVar, filteredDimensions.map(mapping))
