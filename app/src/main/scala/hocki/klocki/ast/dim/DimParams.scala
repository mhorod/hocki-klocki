package hocki.klocki.ast.dim


class DimParams(val universals: List[DimBinding], val existentials: List[DimRef])

object DimParams:
  def empty: DimParams = DimParams(List(), List())