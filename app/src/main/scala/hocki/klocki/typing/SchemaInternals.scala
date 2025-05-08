package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar, Edge}

case class SchemaInternals(localDims: Set[Dim], dimSetVars: Set[DimSetVar], edges: Set[Edge])
