package hocki.klocki.typing

import hocki.klocki.ast.Statement.BlockUse

class Renamer(schemaIface: SchemaIface, useIface: SchemaIface):
  if schemaIface.ins.size != useIface.ins.size then
    throw IllegalArgumentException("Wrong number of input vertices")
  if schemaIface.outs.size != useIface.outs.size then
    throw IllegalArgumentException("Wrong number of output vertices")
  if schemaIface.existentials.size != useIface.existentials.size then
    throw IllegalArgumentException("Wrong number of existential dimensions")
  if schemaIface.universals.size != useIface.existentials.size then
    throw IllegalArgumentException("Wrong number of universal dimensions")

  private val dimMapping = schemaIface.universals.zip(useIface.universals).toMap
    ++ schemaIface.existentials.zip(useIface.existentials).toMap

  private val dimSetVarMapping = schemaIface.ins.zip(useIface.ins).toMap
    ++ schemaIface.outs.zip(useIface.outs).toMap

  def mapConstraints(constraints: Iterable[Constraint]): Iterable[Constraint] =
    constraints.map(_.mapDims(dimMapping).mapDimSetVars(dimSetVarMapping))