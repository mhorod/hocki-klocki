package hocki.klocki.names

import hocki.klocki.semantics.dims.{Dim, DimSetVar}

class SimpleNameGenerator extends NameGenerator:
  private var dimId = 0
  private var inDimSetId = 0
  private var outDimSetId = 0

  override def freshDim(): Dim =
    val result = Dim(s"a$dimId")
    dimId += 1
    result

  override def freshInDimSetVar(): DimSetVar =
    val result = DimSetVar(s"X$inDimSetId")
    inDimSetId += 1
    result

  override def freshOutDimSetVar(): DimSetVar =
    val result = DimSetVar(s"Y$outDimSetId")
    outDimSetId += 1
    result
