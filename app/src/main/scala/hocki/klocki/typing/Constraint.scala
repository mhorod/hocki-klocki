package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

enum Constraint:
  case In(dim: Dim, dimSetVar: DimSetVar)
  case InUnion(dim: Dim, union: Set[DimSetVar])
  case NotIn(dim: Dim, dimSetVar: DimSetVar)
  case DependsOnAll(dim: Dim, filteredDimSetVar: FilteredDimSetVar)
  case DependsOnDim(depender: Dim, dependency: Dim)
  case MinIn(dim: Dim, filteredDimSetVar: FilteredDimSetVar)
  case InducedBy(induced: DimSetVar, inducers: Set[FilteredDimSetVar])

  override def toString: String = this match
    case In(dim, dimSetVar) => s"$dim ∈ $dimSetVar"
    case InUnion(dim, union) => s"$dim ∈ U{${union.mkString(", ")}}"
    case NotIn(dim, dimSetVar) => s"$dim ∉ $dimSetVar"
    case DependsOnAll(dim, filteredDimSetVar) => s"$dim --> $filteredDimSetVar"
    case DependsOnDim(depender, dependency) => s"$depender --> $dependency"
    case MinIn(dim, filteredDimSetVar) => s"$dim ⟂ $filteredDimSetVar"
    case InducedBy(induced, inducers) => s"$induced ⊇ ${inducers.mkString(", ")}"
  
  def mapDimSetVars(mapping: Map[DimSetVar, DimSetVar]): Constraint = this match
    case In(dim, dimSetVar) => In(dim, mapping(dimSetVar))
    case InUnion(dim, union) => InUnion(dim, union.map(mapping))
    case NotIn(dim, dimSetVar) => NotIn(dim, mapping(dimSetVar))
    case DependsOnAll(dim, filteredDimSetVar) => DependsOnAll(dim, filteredDimSetVar.mapDimSetVars(mapping))
    case dependsOnDim: DependsOnDim => dependsOnDim
    case MinIn(dim, filteredDimSetVar) => MinIn(dim, filteredDimSetVar.mapDimSetVars(mapping))
    case InducedBy(induced, inducers) => InducedBy(mapping(induced), inducers.map(_.mapDimSetVars(mapping)))

  def mapDims(mapping: Map[Dim, Dim]): Constraint = this match
    case In(dim, dimSetVar) => In(mapping(dim), dimSetVar)
    case InUnion(dim, union) => InUnion(mapping(dim), union)
    case NotIn(dim, dimSetVar) => NotIn(mapping(dim), dimSetVar)
    case DependsOnAll(dim, filteredDimSetVar) => DependsOnAll(mapping(dim), filteredDimSetVar.mapDims(mapping))
    case dependsOnDim: DependsOnDim => dependsOnDim
    case MinIn(dim, filteredDimSetVar) => MinIn(mapping(dim), filteredDimSetVar.mapDims(mapping))
    case InducedBy(induced, inducers) => InducedBy(induced, inducers.map(_.mapDims(mapping)))

extension (inducedBy: Constraint.InducedBy)
  def inducerDimSetVars: Set[DimSetVar] = inducedBy.inducers.map(_.dimSetVar)