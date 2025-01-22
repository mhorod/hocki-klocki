package hocki.klocki.typing

import hocki.klocki.entities.DimSetVar

class SchemaTy(val ins: List[DimSetVar], val outs: List[DimSetVar], val constraints: Set[Constraint])
