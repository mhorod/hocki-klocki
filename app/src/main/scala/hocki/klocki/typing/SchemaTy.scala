package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

class SchemaTy
(
  val universalDims: List[Dim],
  val existentialDims: List[Dim],
  val ins: List[DimSetVar],
  val outs: List[DimSetVar],
  val constraints: Set[Constraint]
)
