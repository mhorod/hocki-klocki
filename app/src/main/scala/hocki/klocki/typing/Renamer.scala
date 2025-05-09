package hocki.klocki.typing

import hocki.klocki.typing.Constraint.{InductionNamed, InductionUnnamed}

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

  def rename(constraint: Constraint): Set[Constraint] =
    if !constraint.conforms(schemaIface.allDims, schemaIface.allDimSetVars) then
      Set.empty
    else constraint match
      case unnamed: Constraint.InductionUnnamed =>
        dims.all.values.toSet.diff(useIface.allDims)
          .map(dim => InductionNamed(dim, unnamed.from, unnamed.to))
          .map(_.mapDimSetVars(dimSetVarMapping))
          + InductionUnnamed(unnamed.from, unnamed.to).mapDimSetVars(dimSetVarMapping)
      case _ => Set(constraint.mapDims(dimMapping).mapDimSetVars(dimSetVarMapping))

  def rename(constraints: Iterable[Constraint]): Iterable[Constraint] =
    constraints.flatMap(rename)