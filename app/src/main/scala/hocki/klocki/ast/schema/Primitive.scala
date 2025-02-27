package hocki.klocki.ast.schema

import hocki.klocki.ast.dim.DimRef
import hocki.klocki.entities.Dim

enum Primitive:
  case Union(arity: Int)
  case AddNamed(dim: DimRef)
  case AddExistential()
  case Remove()

  override def toString: String =
    val builtinRepr = this match
      case Primitive.Union(arity) => s"U{$arity}"
      case Primitive.AddNamed(dim) => s"+$dim"
      case Primitive.AddExistential() => s"*"
      case Primitive.Remove() => s"-"
    s"builtin $builtinRepr"
