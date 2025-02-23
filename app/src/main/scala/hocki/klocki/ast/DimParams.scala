package hocki.klocki.ast

import hocki.klocki.entities.Dim

class DimParams(universals: List[DimBinding], existential: List[DimBinding])

object DimParams:
  def empty: DimParams = DimParams(List(), List())