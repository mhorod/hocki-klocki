package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

case class SchemaIface
(
  universals: List[Dim],
  existentials: List[Dim],
  ins: List[DimSetVar],
  outs: List[DimSetVar],
):
  lazy val allDimSetVars: Set[DimSetVar] = ins.toSet ++ outs.toSet
  
  lazy val allDims: Set[Dim] = universals.toSet ++ existentials.toSet
