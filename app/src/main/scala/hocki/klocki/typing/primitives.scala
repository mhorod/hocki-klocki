package hocki.klocki.typing

import hocki.klocki.analysis.ResolvedNames
import hocki.klocki.ast.dim.DimBinding
import hocki.klocki.ast.schema.Primitive
import hocki.klocki.entities.{Dim, DimSetVar}

def getTypeOfPrimitive
(primitive: Primitive)
(using
 nr: ResolvedNames,
 globalDims: Map[DimBinding, Dim],
): SchemaTy = primitive match
  case union: Primitive.Union => getTypeOfUnion(union)
  case addNamed: Primitive.AddNamed => getTypeOfAddNamed(addNamed)
  case addExistential: Primitive.AddExistential => throw new IllegalStateException("Existential dims verboten")
  case remove: Primitive.Remove => getTypeOfRemove(remove)

private def getTypeOfUnion(union: Primitive.Union): SchemaTy =
  val xs = (0 until union.arity).map(i => DimSetVar(s"X$i")).toList
  val y = DimSetVar("Y")
  SchemaTy(List(), List(), Set(), xs, List(y), xs.map(_ ~~> y).toSet)

private def getTypeOfAddNamed
(addNamed: Primitive.AddNamed)
(using
  nr: ResolvedNames,
  globalDims: Map[DimBinding, Dim],
): SchemaTy =
  val dim = globalDims(nr.dimNames(addNamed.dim))
  val x = DimSetVar("X")
  val y = DimSetVar("Y")
  val constraints = Set[Constraint](
    dim notIn x,
    dim in y,
    x ~~> y,
  )
  SchemaTy(List(), List(), Set(dim), List(x), List(y), constraints)

private def getTypeOfRemove(remove: Primitive.Remove): SchemaTy =
  val dim = Dim("a")
  val x = DimSetVar("X")
  val y = DimSetVar("Y")
  val constraints = Set[Constraint](
    dim inUnion Set(x),
    x ~~> y,
  )
  SchemaTy(List(dim), List(), Set(dim), List(x), List(y), constraints)