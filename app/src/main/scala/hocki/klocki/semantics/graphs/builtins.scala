package hocki.klocki
package semantics.graphs

import semantics.dims.DimSetVar

def builtinSchema(inCount: Int, outCount: Int): BlockSchema =
  val inVertices = (0 until inCount).map(i => DimSetVar(s"X$i")).toList
  val outVertices = (0 until outCount).map(i => DimSetVar(s"Y$i")).toList
  BlockSchema(inVertices, outVertices, Set(), Set())
