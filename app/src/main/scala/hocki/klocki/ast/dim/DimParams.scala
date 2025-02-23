package hocki.klocki.ast.dim

import hocki.klocki.ast.dim.DimParams
import hocki.klocki.entities.Dim

class DimParams(val universals: List[DimBinding], val existential: List[DimBinding])

object DimParams:
  def empty: DimParams = DimParams(List(), List())