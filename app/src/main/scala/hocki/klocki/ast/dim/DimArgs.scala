package hocki.klocki.ast.dim

import hocki.klocki.ast.dim.DimArgs

class DimArgs(val universals: List[DimRef], val existentials: List[DimRef])

object DimArgs:
  def empty: DimArgs = DimArgs(List(), List())
