package hocki.klocki.typing

import hocki.klocki.ast.schema.SchemaBinding
import hocki.klocki.ast.{Abstra, Binding}
import hocki.klocki.entities.{Dim, DimSetVar}
import hocki.klocki.typing.Constraint
import hocki.klocki.typing.Constraint.In

import scala.collection.mutable
import scala.reflect.ClassTag

private def getConstraints[C <: Constraint](constraints: Set[Constraint])(using classTag: ClassTag[C]): Set[C] =
  constraints.collect { case c if classTag.runtimeClass.isInstance(c) => c.asInstanceOf[C] }

private def filterRelevantConstraints
(
  constraints: Set[Constraint],
  iface: SchemaIface
): Set[Constraint] =
  constraints.filter(_.dimSetVars subsetOf iface.allDimSetVars)

private def getConstraints[C <: Constraint](schemaConstraints: SchemaConstraints)(using classTag: ClassTag[C]): Set[C] =
  getUntaggedConstraints[C](ungroupFromSchema(schemaConstraints))

private def getUntaggedConstraints[C <: Constraint](constraints: Set[SchemaConstraint])(using classTag: ClassTag[C]): Set[C] =
  getConstraints[C](constraints.map(_.constraint))

private def instantiateBound[B <: Binding, E](bindings: Iterable[B], toEntity: String => E): Map[B, E] =
  bindings.map(binding => binding -> toEntity(binding.id.name)).toMap

private def groupBySchema(schemaConstraints: Set[SchemaConstraint]): SchemaConstraints =
  schemaConstraints.groupMap(_.source)(_.constraint)

private def ungroupFromSchema(schemaConstraints: SchemaConstraints): Set[SchemaConstraint] =
  schemaConstraints.flatMap((s, cs) => tagWithSchema(s, cs)).toSet

private def tagWithSchema(binding: SchemaBinding, constraints: Set[Constraint]): Set[SchemaConstraint] =
  constraints.map(SchemaConstraint(binding, _))

private def isSatisfiedUnion(dim: Dim, parts: Set[DimSetVar])(using ins: Set[In]): Boolean =
  parts.exists(part => ins.contains(dim in part))
