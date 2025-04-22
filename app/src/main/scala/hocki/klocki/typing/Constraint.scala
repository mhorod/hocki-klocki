package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

enum Constraint:
  case In(dim: Dim, dimSetVar: DimSetVar)
  case InUnion(dim: Dim, union: Set[DimSetVar])
  case NotIn(dim: Dim, dimSetVar: DimSetVar)
  case DependsOnAll(dim: Dim, filteredDimSetVar: FilteredDimSetVar)
  case DependsOnDim(depender: Dim, dependency: Dim)
  case MinIn(dim: Dim, filteredDimSetVar: FilteredDimSetVar)
  case InductionNamed(dim: Dim, from: DimSetVar, to: DimSetVar)
  case InductionUnnamed(from: DimSetVar, to: DimSetVar)

  override def toString: String = this match
    case In(dim, dimSetVar) => s"$dim ∈ $dimSetVar"
    case InUnion(dim, union) => s"$dim ∈ U{${union.mkString(", ")}}"
    case NotIn(dim, dimSetVar) => s"$dim ∉ $dimSetVar"
    case DependsOnAll(dim, filteredDimSetVar) => s"$dim -> $filteredDimSetVar"
    case DependsOnDim(depender, dependency) => s"$depender -> $dependency"
    case MinIn(dim, filteredDimSetVar) => s"$dim ⟂ $filteredDimSetVar"
    case InductionUnnamed(from, to) => s"$from ==> $to"
    case InductionNamed(dim, from, to) => s"$from ==$dim=> $to"
  
  def mapDimSetVars(mapping: Map[DimSetVar, DimSetVar]): Constraint = this match
    case In(dim, dimSetVar) => In(dim, mapping(dimSetVar))
    case InUnion(dim, union) => InUnion(dim, union.map(mapping))
    case NotIn(dim, dimSetVar) => NotIn(dim, mapping(dimSetVar))
    case DependsOnAll(dim, filteredDimSetVar) => DependsOnAll(dim, filteredDimSetVar.mapDimSetVars(mapping))
    case dependsOnDim: DependsOnDim => dependsOnDim
    case MinIn(dim, filteredDimSetVar) => MinIn(dim, filteredDimSetVar.mapDimSetVars(mapping))
    case InductionNamed(dim, from, to) => InductionNamed(dim, mapping(from), mapping(to))
    case InductionUnnamed(from, to) => InductionUnnamed(mapping(from), mapping(to))

  def mapDims(mapping: Map[Dim, Dim]): Constraint = this match
    case In(dim, dimSetVar) => In(mapping(dim), dimSetVar)
    case InUnion(dim, union) => InUnion(mapping(dim), union)
    case NotIn(dim, dimSetVar) => NotIn(mapping(dim), dimSetVar)
    case DependsOnAll(dim, filteredDimSetVar) => DependsOnAll(mapping(dim), filteredDimSetVar.mapDims(mapping))
    case dependsOnDim: DependsOnDim => dependsOnDim
    case MinIn(dim, filteredDimSetVar) => MinIn(mapping(dim), filteredDimSetVar.mapDims(mapping))
    case InductionNamed(dim, from, to) => InductionNamed(mapping(dim), from, to)
    case InductionUnnamed(from, to) => InductionUnnamed(from, to)
