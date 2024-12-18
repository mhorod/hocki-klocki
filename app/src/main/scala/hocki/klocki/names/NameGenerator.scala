package hocki.klocki
package names

import semantics.dims.{Dim, DimSetVar}

trait NameGenerator:
  def freshDim(): Dim
  def freshInDimSetVar(): DimSetVar
  def freshOutDimSetVar(): DimSetVar
