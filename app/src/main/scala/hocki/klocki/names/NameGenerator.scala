package hocki.klocki.names

import hocki.klocki.entities.{Dim, DimSetVar}

trait NameGenerator:
  def freshDim(): Dim
  def freshInDimSetVar(): DimSetVar
  def freshOutDimSetVar(): DimSetVar
