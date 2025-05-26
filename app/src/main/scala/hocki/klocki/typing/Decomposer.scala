package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

private class Decomposer
(
  ifaceDimSetVars: Set[DimSetVar],
  constraints: SchemaConstraints,
):
  private val inductionsNamed = getConstraints[Constraint.InductionNamed](constraints)
  private val inductionsUnnamed = getConstraints[Constraint.InductionUnnamed](constraints)

  private val inducersNamed =
    inductionsNamed
      .filter(ind => ifaceDimSetVars.contains(ind.from))
      .groupMap(ind => (ind.to, ind.dim))(_.from)

  private val inducersUnnamed =
    inductionsUnnamed
      .filter(ind => ifaceDimSetVars.contains(ind.from))
      .groupMap(_.to)(_.from)

  def decomposeNamed(dim: Dim, dsv: DimSetVar): Set[DimSetVar] =
    inducersNamed((dsv, dim))

  def decomposeNamed(dim: Dim, dsvs: Set[DimSetVar]): Set[DimSetVar] =
    dsvs.flatMap(dsv => decomposeNamed(dim, dsv))

  def decomposeUnnamed(dsv: DimSetVar): Set[DimSetVar] =
    inducersUnnamed(dsv)

  def decomposeUnnamed(dsvs: Set[DimSetVar]): Set[DimSetVar] =
    dsvs.flatMap(dsv => decomposeUnnamed(dsv))