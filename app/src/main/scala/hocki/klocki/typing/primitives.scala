package hocki.klocki.typing

import hocki.klocki.Config
import hocki.klocki.ast.schema.Primitive
import hocki.klocki.entities.{Dim, DimSetVar}
import hocki.klocki.typing.Constraint.NotExistential

def getTypeOfPrimitive(primitive: Primitive): SchemaTy = primitive match
  case Primitive.Union(n) => tysOfUnions(n)
  case Primitive.Add() => tyOfAdd
  case Primitive.Spawn() => tyOfSpawn
  case Primitive.Remove() => tyOfRemove
  case Primitive.Join() => tyOfJoin

private val tysOfUnions: Vector[SchemaTy] = Vector.from((0 until Config.MaxUnionWidth).map(generateTyOfUnion))  

private val tyOfAdd: SchemaTy = generateTyOfExtendWithDim(isUniversalDim = true)

private val tyOfSpawn: SchemaTy = generateTyOfExtendWithDim(isUniversalDim = false)

private val tyOfRemove: SchemaTy =
  val dim = Dim("δ")
  val x = DimSetVar("X")
  val y = DimSetVar("Y")
  val constraints = Set[Constraint](
    dim inUnion Set(x),
    x ~~> y,
  )
  val iface = SchemaIface(List(dim), List(), List(x), List(y))
  SchemaTy(iface, constraints)

private val tyOfJoin: SchemaTy =
  val x0 = DimSetVar("X0")
  val x1 = DimSetVar("X1")
  val y = DimSetVar("Y")
  val constraints = Set(
    Set(x0) <==> Set(x1),
    x0 ~~> y,
    x1 ~~> y,
  )
  val iface = SchemaIface(List(), List(), List(x0, x1), List(y))
  SchemaTy(iface, constraints)

private def generateTyOfExtendWithDim(isUniversalDim: Boolean): SchemaTy =
  val dim = Dim("δ")
  val x = DimSetVar("X")
  val y = DimSetVar("Y")
  val constraints = Set[Constraint](
    dim notIn x,
    dim in y,
    x ~~> y,
  )
  val quantificationConstraints = if isUniversalDim then Set(NotExistential(dim)) else Set()
  val (universals, existentials) = if isUniversalDim then (List(dim), List()) else (List(), List(dim))
  val iface = SchemaIface(universals, existentials, List(x), List(y))
  SchemaTy(iface, constraints ++ quantificationConstraints)

private def generateTyOfUnion(arity: Int): SchemaTy =
  val xs = (0 until arity).map(i => DimSetVar(s"X$i")).toList
  val y = DimSetVar("Y")
  val iface = SchemaIface(List(), List(), xs, List(y))
  SchemaTy(iface, xs.map(_ ~~> y).toSet)
