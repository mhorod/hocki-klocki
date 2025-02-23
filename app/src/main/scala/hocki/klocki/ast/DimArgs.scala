package hocki.klocki.ast

class DimArgs(val universals: List[DimRef], val existentials: List[DimRef])

object DimArgs:
  def empty: DimArgs = DimArgs(List(), List())
