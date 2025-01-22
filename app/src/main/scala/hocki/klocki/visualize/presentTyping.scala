package hocki.klocki.visualize

import hocki.klocki.ast.SchemaBinding
import hocki.klocki.typing.SchemaTy

def presentTyping(typing: Map[SchemaBinding, SchemaTy]): String =
  typing.map((schema, ty) =>
    val ins = ty.ins.mkString(", ")
    val outs = ty.outs.mkString(", ")
    val constraints =
      ty
        .constraints
        .toList
        .sortBy(_.toString)
        .map(constraint => s"    $constraint")
        .mkString("\n")
    s"${schema.id} : [$ins | $outs]\n$constraints"
  ).mkString("\n\n")
