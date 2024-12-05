package hocki.klocki
package typing

import semantics.dims.{Dim, DimSetVar}

enum Constraint:
  case In(dim: Dim, dimSetVar: DimSetVar)
  case InUnion(dim: Dim, union: Set[DimSetVar])
  case NotIn(dim: Dim, dimSetVar: DimSetVar)
  case DependsOn(dim: Dim, filteredDimSetVar: FilteredDimSetVar)
  case InducedBy(induced: DimSetVar, inducers: Set[FilteredDimSetVar])

  override def toString: String = this match
    case In(dim, dimSetVar) => s"$dim ∈ $dimSetVar"
    case InUnion(dim, union) => s"$dim ∈ U{${union.mkString(", ")}}"
    case NotIn(dim, dimSetVar) => s"$dim ∉ $dimSetVar"
    case DependsOn(dim, filteredDimSetVar) => s"$dim --> $filteredDimSetVar"
    case InducedBy(induced, inducers) => s"$induced ⊇ ${inducers.mkString(", ")}"

  def map(mapping: Map[DimSetVar, DimSetVar]): Constraint = this match
    case In(dim, dimSetVar) => In(dim, mapping(dimSetVar))
    case InUnion(dim, union) => InUnion(dim, union.map(mapping))
    case NotIn(dim, dimSetVar) => NotIn(dim, mapping(dimSetVar))
    case DependsOn(dim, filteredDimSetVar) => DependsOn(dim, filteredDimSetVar.map(mapping))
    case InducedBy(induced, inducers) => InducedBy(mapping(induced), inducers.map(_.map(mapping)))

extension (inducedBy: Constraint.InducedBy)
  def inducerDimSetVars: Set[DimSetVar] = inducedBy.inducers.map(_.dimSetVar)