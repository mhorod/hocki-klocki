package hocki.klocki.typing

import hocki.klocki.typing.Constraint.{EquivNamed, EquivUnnamed, InductionNamed, InductionUnnamed}

import scala.collection.mutable

class Renamer(schemaIface: SchemaIface, useIface: SchemaIface, dims: SchemaDims):
  if schemaIface.ins.size != useIface.ins.size then
    throw IllegalArgumentException(s"Wrong number of input vertices $schemaIface, $useIface")
  if schemaIface.outs.size != useIface.outs.size then
    throw IllegalArgumentException(s"Wrong number of output vertices $schemaIface, $useIface")
  if schemaIface.existentials.size != useIface.existentials.size then
    throw IllegalArgumentException(s"Wrong number of existential dimensions $schemaIface, $useIface")
  if schemaIface.universals.size != useIface.universals.size then
    throw IllegalArgumentException(s"Wrong number of universal dimensions $schemaIface, $useIface")

  private val dimMapping = schemaIface.universals.zip(useIface.universals).toMap
    ++ schemaIface.existentials.zip(useIface.existentials).toMap

  private val dimSetVarMapping = schemaIface.ins.zip(useIface.ins).toMap
    ++ schemaIface.outs.zip(useIface.outs).toMap

  private val alreadyRenamed = mutable.Set[Constraint]()

  def rename(constraint: Constraint): Set[Constraint] =
    val renamed =
      if !constraint.conforms(schemaIface.allDims, schemaIface.allDimSetVars) then
        Set.empty
      else if alreadyRenamed.contains(constraint) then
        Set.empty
      else
        alreadyRenamed.add(constraint)
        val dimDiff = dims.all.values.toSet.diff(useIface.allDims)
        constraint match
        case InductionUnnamed(from, to) =>
          (dimDiff.map(dim => InductionNamed(dim, from, to)) + InductionUnnamed(from, to))
          .map(_.mapDimSetVars(dimSetVarMapping))
        case EquivUnnamed(lhs, rhs) =>
          (dimDiff.map(dim => EquivNamed(dim, lhs, rhs)) + EquivUnnamed(lhs, rhs))
            .map(_.mapDimSetVars(dimSetVarMapping))
        case _ => Set(constraint.mapDims(dimMapping).mapDimSetVars(dimSetVarMapping))
    if renamed.nonEmpty then
      println(s"Renaming $constraint --> ${renamed.mkString(", ")}")
    renamed

  def rename(constraints: Iterable[Constraint]): Iterable[Constraint] =
    constraints.flatMap(rename)