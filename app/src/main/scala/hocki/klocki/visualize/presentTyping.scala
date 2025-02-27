package hocki.klocki.visualize

import hocki.klocki.ast.schema.SchemaBinding
import hocki.klocki.typing.{Constraint, SchemaTy}

def presentTyping(typing: Map[SchemaBinding, SchemaTy]): String =
  typing.map((schema, ty) =>
    val universals = ty.universalDims.mkString(", ")
    val existentials = ty.existentialDims.mkString(", ")
    val ins = ty.ins.mkString(", ")
    val outs = ty.outs.mkString(", ")
    val constraints = presentConstraints(ty.constraints)
    s"${schema.id}<$universals | $existentials> : [$ins | $outs]\n$constraints"
  ).mkString("\n\n")

def presentConstraints(constraints: Set[? <: Constraint]): String =
  if constraints.isEmpty then
    "    stole (ukradli)"
  else
    constraints
      .toList
      .sortBy(_.ordinal)
      .map(constraint => s"    $constraint")
      .mkString("\n")

def printConstraints(comment: String, constraints: Set[? <: Constraint]): Unit =
  println(comment)
  println(presentConstraints(constraints))
