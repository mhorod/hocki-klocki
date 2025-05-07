package hocki.klocki.typing

import hocki.klocki.ast.schema.SchemaBinding

import scala.collection.mutable

case class SchemaTyping(
  iface: SchemaIface,
  internals: SchemaInternals,
  renamers: Map[SchemaBinding, Set[Renamer]],
  constraints: mutable.Set[Constraint],
)
