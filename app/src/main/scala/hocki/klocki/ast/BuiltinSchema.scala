package hocki.klocki.ast

import hocki.klocki.entities.Dim

enum BuiltinSchema:
  case Union(arity: Int)
  case AddNamed(dim: DimRef)
  case AddExistential(dim: DimRef)
  case Remove(dim: DimRef)

  override def toString: String =
    val builtinRepr = this match
      case BuiltinSchema.Union(arity) => s"U{$arity}"
      case BuiltinSchema.AddNamed(dim) => s"+$dim"
      case BuiltinSchema.AddExistential(dim) => s"*$dim"
      case BuiltinSchema.Remove(dim) => s"-$dim"
    s"builtin $builtinRepr"
