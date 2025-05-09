package hocki.klocki.typing

import hocki.klocki.ast.{Abstra, Binding}
import hocki.klocki.entities.DimSetVar
import hocki.klocki.typing.Constraint

import scala.collection.mutable
import scala.reflect.ClassTag

private def getConstraints[V <: Constraint](constraints: Set[Constraint])(using classTag: ClassTag[V]): Set[V] =
  constraints.collect { case c if classTag.runtimeClass.isInstance(c) => c.asInstanceOf[V] }

private def filterRelevantConstraints
(
  constraints: Set[Constraint],
  iface: SchemaIface
): Set[Constraint] =
  val ifaceDimSetVars = iface.allDimSetVars
  constraints.filter {
    case Constraint.InductionNamed(_, from, to) => ifaceDimSetVars.contains(from) && ifaceDimSetVars.contains(to)
    case Constraint.InductionUnnamed(from, to) => ifaceDimSetVars.contains(from) && ifaceDimSetVars.contains(to)
    case Constraint.In(_, dsv) => ifaceDimSetVars.contains(dsv)
    case Constraint.NotIn(_, dsv) => ifaceDimSetVars.contains(dsv)
    case Constraint.InUnion(_, union) => union subsetOf ifaceDimSetVars
    case _ => false
  }

private def instantiateBound[B <: Binding, E](bindings: List[B], toEntity: String => E): Map[B, E] =
  bindings.map(binding => binding -> toEntity(binding.id.name)).toMap