package hocki.klocki.semantics.graphs

import hocki.klocki.semantics.dims.DimSetVar

class Block(val schema: BlockSchema, val freshMapping: Map[DimSetVar, DimSetVar])
