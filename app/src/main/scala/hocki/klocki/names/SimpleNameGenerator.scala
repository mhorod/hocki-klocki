package hocki.klocki.names

import hocki.klocki.entities.{Dim, DimSetVar}

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

  override def refresh(dimSetVar: DimSetVar): DimSetVar =
    val name = dimSetVar.name
    if name.startsWith("X") then
      freshInDimSetVar()
    else if name.startsWith("Y") then
      freshOutDimSetVar()
    else
      throw new IllegalArgumentException(s"Unexpected DimSetVar name: $name")