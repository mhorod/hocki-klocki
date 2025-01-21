package hocki.klocki.ast

import hocki.klocki.entities.Dim

enum BuiltinSchema:
  case Union(arity: Int)
  case Add(dim: Dim)
  case Remove(dim: Dim)

  override def toString: String =
    val builtinRepr = this match
      case BuiltinSchema.Union(arity) => s"U{$arity}"
      case BuiltinSchema.Add(dim) => s"+$dim"
      case BuiltinSchema.Remove(dim) => s"-$dim"
    s"builtin $builtinRepr"
