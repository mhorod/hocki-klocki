package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

enum Constraint:
  case NotExistential(dim: Dim)
  case NotIn(dim: Dim, dimSetVar: DimSetVar)
  case InUnion(dim: Dim, union: Set[DimSetVar])
  case DependsOnAll(dim: Dim, filteredDimSetVar: FilteredDimSetVar)
  case DependsOnDim(depender: Dim, dependency: Dim)
  case MinIn(dim: Dim, filteredDimSetVar: FilteredDimSetVar)
  case InductionNamed(dim: Dim, from: DimSetVar, to: DimSetVar)
  case InductionUnnamed(from: DimSetVar, to: DimSetVar)
  case EquivNamed(dim: Dim, lhs: Set[DimSetVar], rhs: Set[DimSetVar])
  case EquivUnnamed(lhs: Set[DimSetVar], rhs: Set[DimSetVar])
  case In(dim: Dim, dimSetVar: DimSetVar)

  override def toString: String = this match
    case In(dim, dimSetVar) => s"$dim ∈ $dimSetVar"
    case InUnion(dim, union) => s"$dim ∈ U{${union.mkString(", ")}}"
    case NotIn(dim, dimSetVar) => s"$dim ∉ $dimSetVar"
    case DependsOnAll(dim, filteredDimSetVar) => s"$dim -> $filteredDimSetVar"
    case DependsOnDim(depender, dependency) => s"$depender -> $dependency"
    case MinIn(dim, filteredDimSetVar) => s"$dim ⟂ $filteredDimSetVar"
    case InductionUnnamed(from, to) => s"$from ==?=> $to"
    case InductionNamed(dim, from, to) => s"$from ==$dim=> $to"
    case EquivNamed(dim: Dim, lhs: Set[DimSetVar], rhs: Set[DimSetVar]) => s"$dim ∈ U{${lhs.mkString(", ")}} <=> U{${rhs.mkString(", ")}} ∋ $dim"
    case EquivUnnamed(lhs: Set[DimSetVar], rhs: Set[DimSetVar]) => s"? ∈ U{${lhs.mkString(", ")}} <=> U{${rhs.mkString(", ")}} ∋ ?"
    case NotExistential(dim) => s"$dim !~ ∃"

  def isTrivial: Boolean = this match
    case InductionNamed(_, from, to) => from == to
    case InductionUnnamed(from, to) => from == to
    case _ => false
  
  def conforms(dims: Set[Dim], dimSetVars: Set[DimSetVar]): Boolean = this match
    case In(dim, dimSetVar) => dims.contains(dim) && dimSetVars.contains(dimSetVar)
    case InUnion(dim, union) => dims.contains(dim) && union.forall(dimSetVars.contains)
    case NotIn(dim, dimSetVar) => dims.contains(dim) && dimSetVars.contains(dimSetVar)
    case DependsOnAll(dim, filteredDimSetVar) => dims.contains(dim) 
      && dimSetVars.contains(filteredDimSetVar.dimSetVar) 
      && filteredDimSetVar.filteredDimensions.forall(dims.contains)
    case DependsOnDim(depender, dependency) => dims.contains(depender) && dims.contains(dependency)
    case MinIn(dim, filteredDimSetVar) => dims.contains(dim)
      && dimSetVars.contains(filteredDimSetVar.dimSetVar)
      && filteredDimSetVar.filteredDimensions.forall(dims.contains)
    case InductionNamed(dim, from, to) => dims.contains(dim) && dimSetVars.contains(from) && dimSetVars.contains(to)
    case InductionUnnamed(from, to) => dimSetVars.contains(from) && dimSetVars.contains(to)
    case NotExistential(dim) => dims.contains(dim)
    case EquivNamed(dim, lhs, rhs) => dims.contains(dim) && (lhs subsetOf dimSetVars) && (rhs subsetOf dimSetVars)
    case EquivUnnamed(lhs, rhs) => (lhs subsetOf dimSetVars) && (rhs subsetOf dimSetVars)

  def mapDimSetVars(mapping: Map[DimSetVar, DimSetVar]): Constraint = this match
    case In(dim, dimSetVar) => In(dim, mapping(dimSetVar))
    case InUnion(dim, union) => InUnion(dim, union.map(mapping))
    case NotIn(dim, dimSetVar) => NotIn(dim, mapping(dimSetVar))
    case DependsOnAll(dim, filteredDimSetVar) => DependsOnAll(dim, filteredDimSetVar.mapDimSetVars(mapping))
    case dependsOnDim: DependsOnDim => dependsOnDim
    case MinIn(dim, filteredDimSetVar) => MinIn(dim, filteredDimSetVar.mapDimSetVars(mapping))
    case InductionNamed(dim, from, to) => InductionNamed(dim, mapping(from), mapping(to))
    case InductionUnnamed(from, to) => InductionUnnamed(mapping(from), mapping(to))
    case EquivNamed(dim, lhs, rhs) => EquivNamed(dim, lhs.map(mapping), rhs.map(mapping))
    case EquivUnnamed(lhs, rhs) => EquivUnnamed(lhs.map(mapping), rhs.map(mapping))
    case notExistential: NotExistential => notExistential

  def mapDims(mapping: Map[Dim, Dim]): Constraint = this match
    case In(dim, dimSetVar) => In(mapping(dim), dimSetVar)
    case InUnion(dim, union) => InUnion(mapping(dim), union)
    case NotIn(dim, dimSetVar) => NotIn(mapping(dim), dimSetVar)
    case DependsOnAll(dim, filteredDimSetVar) => DependsOnAll(mapping(dim), filteredDimSetVar.mapDims(mapping))
    case dependsOnDim: DependsOnDim => dependsOnDim
    case MinIn(dim, filteredDimSetVar) => MinIn(mapping(dim), filteredDimSetVar.mapDims(mapping))
    case InductionNamed(dim, from, to) => InductionNamed(mapping(dim), from, to)
    case InductionUnnamed(from, to) => InductionUnnamed(from, to)
    case NotExistential(dim) => NotExistential(mapping(dim))
    case EquivNamed(dim, lhs, rhs) => EquivNamed(mapping(dim), lhs, rhs)
    case EquivUnnamed(lhs, rhs) => EquivUnnamed(lhs, rhs)

  def dims: Set[Dim] = this match
    case in: In => Set(in.dim)
    case notIn: NotIn => Set(notIn.dim)
    case inUnion: InUnion => Set(inUnion.dim)
    case inductionNamed: InductionNamed => Set(inductionNamed.dim)
    case _: InductionUnnamed => Set()
    case NotExistential(dim) => Set(dim)
    case _ => throw IllegalStateException("We don't want such constraints for now")

  def dimSetVars: Set[DimSetVar] = this match
    case in: In => Set(in.dimSetVar)
    case notIn: NotIn => Set(notIn.dimSetVar)
    case inUnion: InUnion => inUnion.union
    case inductionNamed: InductionNamed => Set(inductionNamed.from, inductionNamed.to)
    case inductionUnnamed: InductionUnnamed => Set(inductionUnnamed.from, inductionUnnamed.to)
    case _: NotExistential => Set()
    case _ => throw IllegalStateException("We don't want such constraints for now")
