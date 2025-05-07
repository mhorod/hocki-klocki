package hocki.klocki.typing

import hocki.klocki.entities.{Dim, DimSetVar}

case class SchemaTy(iface: SchemaIface, constraints: Set[Constraint])
