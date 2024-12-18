package hocki.klocki
package semantics.graphs

import semantics.dims.DimSetVar

class Block(val schema: BlockSchema, val freshMapping: Map[DimSetVar, DimSetVar])
