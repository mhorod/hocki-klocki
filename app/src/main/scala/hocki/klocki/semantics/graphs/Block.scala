package hocki.klocki.semantics.graphs

import hocki.klocki.entities.DimSetVar

class Block(val schema: BlockSchema, val freshMapping: Map[DimSetVar, DimSetVar])
