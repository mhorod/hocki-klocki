package hocki.klocki.ast.dim

class DimArgs(val universals: List[DimRef], val existentials: List[DimBinding])

object DimArgs:
  def empty: DimArgs = DimArgs(List(), List())
