package hocki.klocki.ast.schema

import hocki.klocki.ast.dim.DimRef
import hocki.klocki.entities.Dim

enum Primitive:
  case Union(arity: Int)
  case Add()
  case Spawn()
  case Remove()

  override def toString: String =
    val builtinRepr = this match
      case Primitive.Union(arity) => s"U{$arity}"
      case Primitive.Add() => s"+"
      case Primitive.Spawn() => s"*"
      case Primitive.Remove() => s"-"
    s"builtin $builtinRepr"
