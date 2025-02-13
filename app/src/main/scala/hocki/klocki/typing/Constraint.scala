package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

enum Constraint:
  case NotIn(dim: Dim, dimSetVar: DimSetVar)
  case IsMin(dim: Dim, filteredDimSetVar: FilteredDimSetVar)
  case InducedBy(induced: DimSetVar, inducers: Set[FilteredDimSetVar])
  case DependsOnAll(dim: Dim, dependency: FilteredDimSetVar, ctx: DimSetVar)
  case DependsOnDim(dim: Dim, dependency: Dim, ctx: DimSetVar)
  case InUnion(dim: Dim, union: Set[DimSetVar])

  override def toString: String = this match
    case NotIn(dim, dimSetVar) => s"$dim ∉ $dimSetVar"
    case IsMin(dim, filteredDimSetVar) => s"$dim ⊥ $filteredDimSetVar"
    case InducedBy(induced, inducers) => s"$induced ⊇ ${inducers.mkString(", ")}"
    case DependsOnAll(dim, dependency, ctx) => s"$dim -$ctx-> $dependency"
    case DependsOnDim(dim, dependency, ctx) => s"$dim -$ctx-> $dependency"
    case InUnion(dim, union) => s"$dim ∈ U{${union.mkString(", ")}}"

  def map(mapping: Map[DimSetVar, DimSetVar]): Constraint = this match
    case NotIn(dim, dimSetVar) => NotIn(dim, mapping(dimSetVar))
    case IsMin(dim, filteredDimSetVar) => IsMin(dim, filteredDimSetVar.map(mapping))
    case InducedBy(induced, inducers) => InducedBy(mapping(induced), inducers.map(_.map(mapping)))
    case DependsOnAll(dim, dependency, ctx) => DependsOnAll(dim, dependency.map(mapping), mapping(ctx))
    case DependsOnDim(dim, dependency, ctx) => DependsOnDim(dim, dependency, mapping(ctx))
    case InUnion(dim, union) => InUnion(dim, union.map(mapping))

extension (inducedBy: Constraint.InducedBy)
  def inducerDimSetVars: Set[DimSetVar] = inducedBy.inducers.map(_.dimSetVar)
