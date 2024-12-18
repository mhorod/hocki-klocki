package hocki.klocki.names

import hocki.klocki.semantics.dims.{Dim, DimSetVar}

trait NameGenerator:
  def freshDim(): Dim
  def freshInDimSetVar(): DimSetVar
  def freshOutDimSetVar(): DimSetVar
