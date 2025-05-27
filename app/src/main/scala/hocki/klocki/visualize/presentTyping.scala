package hocki.klocki.visualize

import hocki.klocki.ast.schema.SchemaBinding
import hocki.klocki.typing.{Constraint, SchemaTy}

def presentTyping(typing: Map[SchemaBinding, SchemaTy]): String =
  typing.map((schema, ty) =>
    val universals = ty.iface.universals.mkString(", ")
    val existentials = ty.iface.existentials.mkString(", ")
    val ins = ty.iface.ins.mkString(", ")
    val outs = ty.iface.outs.mkString(", ")
    val constraints = presentConstraints(ty.constraints)
    s"${schema.id} : <$universals | $existentials> [$ins | $outs]\n$constraints"
  ).mkString("\n\n")

def presentConstraints(constraints: Set[? <: Constraint]): String =
  if constraints.isEmpty then
    "  stole (ukradli)" // https://youtu.be/4TxJkjz2HFI?feature=shared ; timestamp 0:20
  else
    constraints
      .toList
      .sortBy(_.ordinal)
      .filterNot(_.isTrivial)
      .map(constraint => s"    $constraint")
      .mkString("\n")

def printConstraints(comment: String, constraints: Set[? <: Constraint]): Unit =
  println(comment)
  println(presentConstraints(constraints))
