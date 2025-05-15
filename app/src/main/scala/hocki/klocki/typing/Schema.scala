package hocki.klocki.typing

import hocki.klocki.ast.schema.SchemaBinding
import hocki.klocki.entities.{Dim, DimSetVar}

import scala.collection.mutable

case class Schema
(
  iface: SchemaIface,
  internals: SchemaInternals,
  renamers: Map[SchemaBinding, Set[Renamer]],
):
  lazy val allDims: Set[Dim] = iface.allDims ++ internals.localDims

  lazy val allExistentials: Set[Dim] = iface.existentials.toSet ++ internals.localDims

  lazy val allDimSetVars: Set[DimSetVar] = iface.allDimSetVars ++ internals.dimSetVars
