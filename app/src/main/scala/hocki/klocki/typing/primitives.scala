package hocki.klocki.typing

import hocki.klocki.Config
import hocki.klocki.analysis.ResolvedNames
import hocki.klocki.ast.dim.DimBinding
import hocki.klocki.ast.schema.Primitive
import hocki.klocki.entities.{Dim, DimSetVar}

def getTypeOfPrimitive(primitive: Primitive): SchemaTy = primitive match
  case Primitive.Union(n) => tysOfUnions(n)
  case Primitive.Add() => tyOfAdd
  case Primitive.Spawn() => tyOfSpawn
  case Primitive.Remove() => tyOfRemove

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

private def generateTyOfExtendWithDim(isUniversalDim: Boolean): SchemaTy =
  val dim = Dim("δ")
  val x = DimSetVar("X")
  val y = DimSetVar("Y")
  val constraints = Set[Constraint](
    dim notIn x,
    dim in y,
    x ~~> y,
  )
  val (universals, existentials) = if isUniversalDim then (List(dim), List()) else (List(), List(dim))
  val iface = SchemaIface(universals, existentials, List(x), List(y))
  SchemaTy(iface, constraints)

private def generateTyOfUnion(arity: Int): SchemaTy =
  val xs = (0 until arity).map(i => DimSetVar(s"X$i")).toList
  val y = DimSetVar("Y")
  val iface = SchemaIface(List(), List(), xs, List(y))
  SchemaTy(iface, xs.map(_ ~~> y).toSet)
